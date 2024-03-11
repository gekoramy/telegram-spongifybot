{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Arrow ((>>>))
import Control.Concurrent
import Control.Concurrent.STM
import Data.Aeson (KeyValue ((.=)), Value, object)
import Data.Aeson.Schema
import qualified Data.Char as Char
import Network.HTTP.Client.Conduit (responseTimeoutMicro)
import Network.HTTP.Simple
import System.Environment (getEnv)
import Universum hiding (atomically, get)

type Updates =
  [schema|
    {
      ok: Bool,
      result: List {
        update_id: Int64,
        message: Maybe {
          chat: {
            id: Int64,
          },
          message_id: Int64,
          text: Maybe Text,
        },
        inline_query: Maybe {
          id: Text,
          query: Text,
        },
      },
    }
  |]

data Req = Req
  { prefix :: ByteString
  , base :: Request
  }

data In = In
  { newMs :: Enqueue (NonEmpty M)
  , newQs :: Enqueue (NonEmpty Q)
  }

data M = M
  { chat :: Int64
  , message :: Int64
  , text :: Maybe Text
  }
  deriving (Generic)

data Q = Q
  { query :: Text
  , text :: Text
  }
  deriving (Generic)

data Method = Method
  { name :: ByteString
  , body :: Value
  }
  deriving (Generic)

instance NFData Method

instance NFData M

instance NFData Q

type Enqueue a = a -> IO ()

type Dequeue a = IO a

infixl 0 |>

(|>) :: a -> (a -> b) -> b
(|>) = (&)

main :: IO ()
main = do
  n <- getNumCapabilities
  putTextLn $ "# of capabilities " <> show n

  token <- getEnv "BOT"
  base <- parseRequest "POST https://api.telegram.org"
  let r = Req {prefix = "/bot" <> fromString token, base = base}
  let toRequest' = toRequest r

  qs <- newTBQueueIO 1000
  ms <- newTBQueueIO 1000
  _ <- forkIO . forever $ dequeue qs <&> map (onQ >>> toRequest') >>= (`forM_` (\x -> insist (httpNoBody x)))
  _ <- forkIO . forever $ dequeue ms <&> map (onM >>> toRequest') >>= (`forM_` (\x -> insist (httpNoBody x) >> threadDelay (1000 * 1000)))

  fetch r In {newMs = enqueue ms, newQs = enqueue qs}

toRequest :: Req -> Method -> Request
toRequest Req {..} Method {..} =
  base
    |> setRequestPath (prefix <> "/" <> name)
    |> setRequestBodyJSON body

dequeue :: TBQueue a -> Dequeue a
dequeue = atomically . readTBQueue

enqueue :: (NFData a) => TBQueue a -> Enqueue a
enqueue q x = evaluateNF x >>= (atomically . writeTBQueue q)

insist :: IO a -> IO a
insist fn = catchAny fn attempt
  where
    attempt e = do
      print e
      threadDelay (1000 * 1000)
      insist fn

onQ :: Q -> Method
onQ Q {..} =
  Method
    { name = "answerInlineQuery"
    , body = inline query $ spongify text
    }

onM :: M -> Method
onM M {..} = case text of
  Nothing ->
    Method
      { name = "setMessageReaction"
      , body = object ["chat_id" .= chat, "message_id" .= message, "reaction" .= [reaction], "is_big" .= True]
      }
  Just "/start" ->
    Method
      { name = "sendVideo"
      , body = object ["chat_id" .= chat, "video" .= origin, "reply_markup" .= switch ""]
      }
  Just "/donate" ->
    Method
      { name = "sendVideo"
      , body = object ["chat_id" .= chat, "video" .= society, "reply_markup" .= donate]
      }
  Just txt ->
    Method
      { name = "sendMessage"
      , body = object ["chat_id" .= chat, "text" .= spongify txt, "reply_markup" .= switch txt]
      }

fetch :: Req -> In -> IO ()
fetch Req {..} In {..} = iterateM_ go Nothing
  where
    go :: Maybe Int64 -> IO (Maybe Int64)
    go offset = do
      res <- insist . httpJSON $ req offset

      let jsn = getResponseBody res :: Object Updates
      let ms =
            jsn
              |> [get| .result[].message?.(chat.id, message_id, text) |]
              |> catMaybes
              |> map (\(c, m, t) -> M {chat = c, message = m, text = t})
      let qs =
            jsn
              |> [get| .result[].inline_query?.(id, query) |]
              |> catMaybes
              |> map (\(q, t) -> Q {query = q, text = t})
      let latest =
            jsn
              |> [get| .result[].update_id |]
              |> nonEmpty
              |> map (last >>> (+ 1))

      whenNotNull qs newQs
      whenNotNull ms newMs
      return latest

    req :: Maybe Int64 -> Request
    req offset =
      base'
        |> setRequestBodyJSON
          ( object
              [ "timeout" .= timeout
              , "allowed_updates" .= (["message", "inline_query"] :: [Text])
              , "offset" .= offset
              ]
          )

    base' :: Request
    base' =
      base
        |> setRequestPath (prefix <> "/getUpdates")
        |> setRequestResponseTimeout (responseTimeoutMicro $ (timeout + 5) * 1000 * 1000)

    timeout :: Int
    timeout = 50

iterateM_ :: (Monad m) => (a -> m a) -> a -> m b
iterateM_ f = g
  where
    g x = f x >>= g

inline :: Text -> Text -> Value
inline query "" =
  object
    [ "inline_query_id" .= query
    , "cache_time" .= (300 :: Int)
    , "results" .= ([] :: [Value])
    ]
inline query s =
  object
    [ "inline_query_id" .= query
    , "cache_time" .= (300 :: Int)
    , "results"
        .= [ object
              [ "id" .= (query <> "img")
              , "type" .= ("article" :: Text)
              , "title" .= spongify "spongify this"
              , "description" .= s
              , "thumbnail_url" .= thumbnail
              , "input_message_content"
                  .= object
                    [ "message_text" .= s
                    , "link_preview_options"
                        .= object
                          [ "url" .= thumbnail
                          , "prefer_large_media" .= True
                          , "show_above_text" .= True
                          ]
                    ]
              ]
           , object
              [ "id" .= query
              , "type" .= ("article" :: Text)
              , "title" .= spongify "spongify this"
              , "description" .= s
              , "input_message_content" .= object ["message_text" .= s]
              ]
           ]
    ]

thumbnail :: Text
thumbnail = "https://cdn4.cdn-telegram.org/file/uiZxrFsnP8tJRDchpW1LnnblgF-Acl_J_2vQmI6NJsHAuE-L2vLZHSd5zQNY4r2SX_Qz7OdA9ECgAsMI_EQl8UX9k6zDhU9ch6xstd5VDBPycXF1SrP7Vr_N4wrn6PpJz1mPRBZzIuTIP5Zm_2xg4j2oswXHebMEOl-t0gu-rJiDKc15HWxHxmm6lqnNGREHy2t60WbD-REOP9IB-6-M3QvgSdjgu3pdpi-fK61nBsbWgB3n4IHC-z0JHehtD9FxCkdx282uhLrR_wNiU7icYm0NxFH2xPQq2n4RoMy0l1jLIaKQIBPlTmGpGOkODa4bH1l6ykN-cM-TIxACfHr_ZA.jpg"

society :: Text
society = "BAACAgQAAxkBAAIDwmXiP928s-G-MSCC0jyhgnwHm6KJAAILEAAC0EoZU2p6d2hsfNnFNAQ"

origin :: Text
origin = "BAADBAADVwUAAlLhUFJOdQxIyHGRPQI"

reaction :: Value
reaction = object ["type" .= ("emoji" :: Text), "emoji" .= ("🗿" :: Text)]

donate :: Value
donate =
  object
    [ "inline_keyboard"
        .= [
             [ object
                [ "text" .= spongify "spare change ma'am?"
                , "url" .= ("https://www.paypal.me/LucaMosetti" :: Text)
                ]
             ]
           ]
    ]

switch :: Text -> Value
switch query =
  object
    [ "inline_keyboard"
        .= [
             [ object
                [ "text" .= spongify "spongify friends!"
                , "switch_inline_query" .= query
                ]
             ]
           ]
    ]

spongify :: Text -> Text
spongify =
  words
    >>> map toString
    >>> map (zipWith ($) $ cycle [Char.toLower, Char.toUpper])
    >>> map fromString
    >>> unwords
