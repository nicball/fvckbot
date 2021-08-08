module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, IOException, catch, handle, throwIO)
import Control.Lens (folded, maximumOf, (.~), (^.), (^..), (^?), (^?!))
import Data.Aeson (Value, encode, object, toJSON)
import Data.Aeson.Lens (key, values, _Array, _Bool, _Integer, _String)
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.Foldable (traverse_)
import Data.Function (fix, (&))
import Data.Maybe (fromJust)
import Data.Monoid (First (..))
import Data.Text (Text, isPrefixOf)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.SQLite.Simple
  ( Connection,
    execute,
    execute_,
    Only (..),
    open,
    Query (..),
    query,
    query_,
    ResultError,
    SQLError,
  )
import Network.HTTP.Simple
  ( getResponseBody,
    httpBS,
    HttpException,
    httpJSON,
    parseRequest_,
    Proxy (..),
    Request,
    Response,
    setRequestHeader,
    setRequestProxy,
    setRequestQueryString,
  )
import System.IO.Unsafe (unsafePerformIO)
import System.Random (getStdRandom, randomR)
import System.Environment (getEnv)

{-# NOINLINE botToken #-}
botToken :: String
botToken = unsafePerformIO $ getEnv "TG_BOT_TOKEN"

botURL :: String
botURL = "https://api.telegram.org/bot" <> botToken

httpProxy :: Proxy
httpProxy = Proxy "127.0.0.1" 1081

newtype TgApiException = TgApiException (Response Value)
  deriving (Show, Exception)

toUft8 :: Show a => a -> BS.ByteString
toUft8 = encodeUtf8 . Text.pack . show

httpJSONExn :: Request -> IO Value
httpJSONExn req = do
  res <- httpJSON req
  let body = getResponseBody res
  if body ^?! key "ok" . _Bool
    then pure body
    else throwIO . TgApiException $ res

getUpdates :: Maybe Integer -> IO [Value]
getUpdates offset = do
  let req =
        setRequestProxy (Just httpProxy)
          . setRequestQueryString [("offset", toUft8 <$> offset)]
          . parseRequest_
          $ botURL <> "/getUpdates"
  body <- httpJSONExn req
  pure $ body ^.. key "result" . values

processUpdate :: Value -> IO ()
processUpdate update = maybe (pure ()) processMessage $ update ^? key "message"

processMessage :: Value -> IO ()
processMessage msg = do
  let mid = msg ^?! key "message_id" . _Integer
      cid = msg ^?! key "chat" . key "id" . _Integer
  case msg ^? key "text" . _String of
    Just text -> processCommand text >>= maybe (pure ()) (sendMessage cid mid)
    Nothing -> pure ()

processCommand :: Text -> IO (Maybe Text)
processCommand text = do
  let handlers = [getIp, ping, pia, rem, dump, wat, getAnswer]
  -- results <- traverse ((First <$>) . ($ text)) handlers
  -- pure . fromJust . getFirst . mconcat $ results
  fmap getFirst . ($ text) . mconcat . (fmap . fmap . fmap $ First) $ handlers
  where
    wat text =
      if not . Text.null . snd . Text.breakOn "@fvckbot" $ text
        then pure . Just $ "wat?"
        else pure Nothing
    getIp = check "/get_ip" . const $ Just <$> getMyIp
    ping = check "/ping" . const . pure . Just $ "ping你妹"
    pia = check "/pia" getAnswer
    rem = check "/rem" $ \text -> do
      let (bef', aft') = Text.breakOn "->" text
          bef = Text.strip bef'
          aft = Text.strip . Text.drop 2 $ aft'
          (q, a) = if Text.null aft then ("", bef) else (bef, aft)
      (setAnswer q a >> pure (Just "朕悉"))
        `catch` (\e -> pure . Just . Text.pack . show $ (e :: SQLError))
    dump = check "/dump" . const $ Just <$> dumpDatabase
    check cmd f = \text ->
      if cmd `isPrefixOf` text
        then f . skipWord $ text
        else pure Nothing
    skipWord = Text.stripStart . Text.dropWhile (not . isSpace)

getAnswer :: Text -> IO (Maybe Text)
getAnswer question = do
  results <- fmap fromOnly <$> query sqlConn "select a from pia where q=?" (Only question)
  if null results
    then pure Nothing
    else Just <$> pick results
  where
    pick xs = (xs !!) <$> getStdRandom (randomR (0, length xs - 1))

setAnswer :: Text -> Text -> IO ()
setAnswer question answer =
  execute sqlConn "insert into pia values (?, ?)" (question, answer)

dumpDatabase :: IO Text
dumpDatabase = do
  results <- query_ sqlConn "select * from pia"
  let msg = mconcat . map (\(q, a) -> q <> " -> " <> a <> "\n") $ results
  if Text.null msg
    then pure "无条目"
    else pure msg

{-# NOINLINE sqlConn #-}
sqlConn :: Connection
sqlConn = unsafePerformIO do
  conn <- open "./fvckbot.db"
  execute_ conn "create table if not exists pia (q text, a text, unique (q, a))"
  execute_ conn "create table if not exists updates (id integer, json text, primary key (id))"
  pure conn

logUpdate :: Value -> IO ()
logUpdate json =
  execute sqlConn "insert into updates values (?, ?)" (json ^?! key "update_id" . _Integer, encode json)
    `catch` (\e -> putStr "logUpdate: " >> print (e :: SQLError))

sendMessage :: Integer -> Integer -> Text -> IO ()
sendMessage chatId replyToMessageId text = do
  httpJSONExn
    . setRequestProxy (Just httpProxy)
    . setRequestQueryString
      [ ("chat_id", Just . toUft8 $ chatId),
        ("text", Just . encodeUtf8 $ text),
        ("reply_to_message_id", Just . toUft8 $ replyToMessageId)
      ]
    . parseRequest_
    $ botURL <> "/sendMessage"
  pure ()

getMyIp :: IO Text
getMyIp = do
  let req = setRequestHeader "User-Agent" ["curl/6.6.6"] . parseRequest_ $ "http://ifconfig.co"
  res <- httpBS req
  pure . decodeUtf8 . getResponseBody $ res

main :: IO ()
main = flip fix Nothing \loop offset ->
  handle (\e -> print (e :: HttpException) >> loop Nothing) $
    handle (\e -> print (e :: TgApiException) >> loop Nothing) do
      upds <- getUpdates offset
      traverse_ logUpdate upds
      traverse_ (handle (print :: HttpException -> IO ()) . processUpdate) upds
      threadDelay 2000000
      loop . fmap (+ 1) . maximumOf (folded . key "update_id" . _Integer) $ upds
