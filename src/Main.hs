module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (handle, Exception, throwIO, IOException, catch)
import Control.Lens ((^?), (^?!), (.~), (^.), maximumOf, folded)
import Data.Aeson (Value, object, toJSON, encode)
import Data.Aeson.Lens (key, _Array, _Bool, _String, _Integer)
import Data.Char (isSpace)
import Data.Foldable (traverse_)
import Data.Function ((&), fix)
import Data.Maybe (fromJust)
import Data.Monoid (First(..))
import Data.Text (pack, isPrefixOf, Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Vector (toList)
import Database.SQLite.Simple (query, query_, execute, execute_, withConnection, Connection, ResultError, Only(..), SQLError)
import GHC.Generics (Generic)
import Network.HTTP.Client (Proxy(..), Response)
import Network.HTTP.Req (http, https, (/:), defaultHttpConfig, req, GET(..), runReq, jsonResponse, JsonResponse, Url, Scheme(..), responseBody
    , NoReqBody(..), queryParam, (=:), ignoreResponse, header, bsResponse, HttpConfig(..), HttpException, toVanillaResponse
    )
import System.Random (randomR, getStdRandom)

import qualified Data.Text as Text

botURL :: Url 'Https
botURL = https "api.telegram.org" /: "bot103568303:AAHmQQfMDnpOdSlTpdyjfhFAcHAOFOag6vI"

proxyHttpConfig :: HttpConfig
proxyHttpConfig = defaultHttpConfig { httpConfigProxy = Just (Proxy "localhost" 1081) }

newtype TgApiException = TgApiException (Response Value)
    deriving (Show)

instance Exception TgApiException

getUpdates :: Maybe Integer -> IO [Value]
getUpdates offset = do
    res <- runReq proxyHttpConfig $ req GET (botURL /: "getUpdates") NoReqBody jsonResponse (queryParam "offset" offset)
    let body = responseBody res
    if body ^? key "ok" . _Bool == Just True
        then pure . toList $ body ^?! key "result" . _Array
        else throwIO . TgApiException . toVanillaResponse $ res

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
            let (q, a) = if Text.null aft then ("", bef) else (bef, aft)
            (setAnswer q a >> pure (Just "朕悉"))
                `catch` 
                (\e -> pure . Just . Text.pack . show $ (e :: SQLError))
        dump = check "/dump" . const $ Just <$> dumpDatabase
        check cmd f = \text -> if cmd `isPrefixOf` text
            then f . skipWord $ text
            else pure Nothing
        skipWord = Text.stripStart . Text.dropWhile (not . isSpace)

getAnswer :: Text -> IO (Maybe Text)
getAnswer question =
    withPia \conn -> do
    results <- fmap fromOnly <$> query conn "select a from pia where q=?" (Only question)
    if null results
        then pure Nothing
        else Just <$> pick results
    where
        pick xs = (xs !!) <$> getStdRandom (randomR (0, length xs - 1))

setAnswer :: Text -> Text -> IO ()
setAnswer question answer = withPia \conn ->
    execute conn "insert into pia values (?, ?)" (question, answer)

dumpDatabase :: IO Text
dumpDatabase = withPia \conn -> do
    results <- query_ conn "select * from pia"
    let msg = mconcat . map (\(q, a) -> q <> " -> " <> a <> "\n") $ results
    if Text.null msg
        then pure "无条目"
        else pure msg

withPia :: (Connection -> IO a) -> IO a
withPia f = withConnection "./fvckbot.db" \conn -> do
    execute_ conn "create table if not exists pia (q text, a text, unique (q, a))"
    f conn

withHistory :: (Connection -> IO a) -> IO a
withHistory f = withConnection "./fvckbot.db" \conn -> do
    execute_ conn "create table if not exists updates (id integer, json text, primary key (id))"
    f conn

logUpdate :: Value -> IO ()
logUpdate json = withHistory \conn ->
    execute conn "insert into updates values (?, ?)" (json ^?! key "update_id" . _Integer, encode $ json)
    `catch` 
    (\e -> putStr "logUpdate: " >> print (e :: SQLError))

sendMessage :: Integer -> Integer -> Text -> IO ()
sendMessage chatId replyToMessageId text = do
    runReq proxyHttpConfig $ req GET (botURL /: "sendMessage") NoReqBody ignoreResponse
        ("chat_id" =: chatId <> "text" =: text <> "reply_to_message_id" =: replyToMessageId)
    pure ()

getMyIp :: IO Text
getMyIp = do
    ip <- runReq defaultHttpConfig $ req GET (http "ifconfig.co") NoReqBody bsResponse (header "User-Agent" "curl/6.6.6")
    pure . decodeUtf8 . responseBody $ ip

main :: IO ()
main = flip fix Nothing \loop offset ->
    handle (\e -> print (e :: HttpException) >> loop Nothing) $
    handle (\e -> print (e :: TgApiException) >> loop Nothing) do
    upds <- getUpdates offset
    traverse_ logUpdate upds
    traverse_ (handle (print :: HttpException -> IO ()) . processUpdate) upds
    threadDelay 2000000
    loop . fmap (+ 1) . maximumOf (folded . key "update_id" . _Integer) $ upds
