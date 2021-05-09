module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (handle, Exception, throwIO)
import Control.Lens ((^?), (^?!), (.~), (^.), maximumOf, folded)
-- import Control.Monad.Except (runExceptT, throwError, lift)
import Data.Aeson (Value, object, toJSON)
import Data.Aeson.Lens (key, _Array, _Bool, _Integral, _String)
import Data.Foldable (traverse_)
import Data.Function ((&), fix)
import Data.Maybe (fromJust)
import Data.Monoid (First(..))
import Data.Text (pack, isPrefixOf, Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Vector (toList)
import Database.SQLite.Simple
import Network.HTTP.Client (Proxy(..), Response)
import Network.HTTP.Req (http, https, (/:), defaultHttpConfig, req, GET(..), runReq, jsonResponse, JsonResponse, Url, Scheme(..), responseBody
    , NoReqBody(..), queryParam, (=:), ignoreResponse, header, bsResponse, HttpConfig(..), HttpException, toVanillaResponse
    )

botURL :: Url 'Https
botURL = https "api.telegram.org" /: "bot103568303:AAHmQQfMDnpOdSlTpdyjfhFAcHAOFOag6vI"

proxyHttpConfig :: HttpConfig
proxyHttpConfig = defaultHttpConfig { httpConfigProxy = Just (Proxy "localhost" 1081) }

newtype TgApiException = TgApiException (Response Value)
    deriving (Show)

instance Exception TgApiException

getUpdates :: Maybe Int -> IO [Value]
getUpdates offset = do
    res <- runReq proxyHttpConfig $ req GET (botURL /: "getUpdates") NoReqBody jsonResponse (queryParam "offset" offset)
    let body = responseBody res
    if body ^? key "ok" . _Bool == Just True
        then pure . toList $ body ^?! key "result" . _Array
        else throwIO . TgApiException . toVanillaResponse $ res

processUpdate :: Value -> IO ()
processUpdate update = do
    case update ^? key "message" of
        Just msg -> processMessage msg
        Nothing -> putStrLn "Update is not a message, ignored:" >> print update

processMessage :: Value -> IO ()
processMessage msg = do
    let mid = msg ^?! key "message_id" . _Integral
        cid = msg ^?! key "chat" . key "id" . _Integral
    case msg ^? key "text" . _String of
        Nothing -> putStrLn "Message is not text, ignored:" >> print msg
        Just text -> processCommand text >>= sendMessage cid mid

processCommand :: Text -> IO Text
processCommand text = do
    let handlers = [getIp, ping, wat]
    -- results <- traverse ((First <$>) . ($ text)) handlers
    -- pure . fromJust . getFirst . mconcat $ results
    fmap (fromJust . getFirst) . ($ text) . mconcat . (fmap . fmap . fmap $ First) $ handlers
    where
        wat _ = pure . Just $ "wat?"
        getIp text = if "/get_ip" `isPrefixOf` text
            then Just <$> getMyIp
            else pure Nothing
        ping text = if "/ping" `isPrefixOf` text
            then pure . Just $ "ping你妹"
            else pure Nothing

sendMessage :: Int -> Int -> Text -> IO ()
sendMessage chatId replyToMessageId text = do
    runReq proxyHttpConfig $ req GET (botURL /: "sendMessage") NoReqBody ignoreResponse
        ("chat_id" =: chatId <> "text" =: text <> "reply_to_message_id" =: replyToMessageId)
    pure ()

getMyIp :: IO Text
getMyIp = do
    ip <- runReq defaultHttpConfig $ req GET (http "ifconfig.co") NoReqBody bsResponse (header "User-Agent" "curl/6.6.6")
    pure . decodeUtf8 . responseBody $ ip

main :: IO ()
main = flip fix Nothing $ \loop offset ->
    handle (\e -> print (e :: HttpException) >> loop Nothing) $
    handle (\e -> print (e :: TgApiException) >> loop Nothing) $
    do
    upds <- getUpdates offset
    -- print upds
    traverse_ (handle (print :: HttpException -> IO ()) . processUpdate) upds
    threadDelay 2000000
    loop . fmap (+ 1) . maximumOf (folded . key "update_id" . _Integral) $ upds
