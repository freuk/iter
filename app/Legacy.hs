{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Legacy (llmLegacy) where

import Common
import Data.Aeson ((.=))
import Data.Aeson as Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson (decode, object)
import qualified Data.ByteString.Lazy.Char8 as C8 (lines)
import qualified Data.Text as T (pack)
import qualified Data.Time.Clock as Time (UTCTime)
import qualified Data.Time.Clock.POSIX as Time (getCurrentTime, utcTimeToPOSIXSeconds)
import qualified Data.Time.Format as Time (defaultTimeLocale, formatTime)
import qualified Network.HTTP.Simple as HTTP
  ( getResponseBody,
    httpLBS,
    setRequestBodyJSON,
  )
import Protolude
import qualified Text.Printf as Printf (printf)

data HistoryMessage = HistoryMessage
  { userPrompt :: Text,
    assistantResponse :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.ToJSON)

newtype Result a = Result {result :: a}
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON)

newtype Content = Content {content :: Text}
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON)

newtype Stats = Stats {stats :: PerformanceStats}
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON)

data PerformanceStats = PerformanceStats
  { timeGenerated :: Double,
    tokensGenerated :: Int,
    timeProcessed :: Double,
    tokensProcessed :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON)

-- | same as llmOpenAiApi, but wants a legacy request token, and offers
-- performance information.
llmLegacy :: [(Text, Text)] -> [Text] -> [Text] -> IterT Text
llmLegacy historyMessages (unlines -> sysPrompt) (unlines -> userPrompt) = do
  request <- do
    body <- do
      model <- optsModel <$> lift ask
      pure
        [ "model_id" .= model,
          "system_prompt" .= sysPrompt,
          "user_prompt" .= userPrompt,
          "history" .= mkHistorymessages historyMessages
        ]
    token <- optsToken <$> lift ask
    pure
      . setBearer token
      $ HTTP.setRequestBodyJSON
        (Aeson.object body)
        "POST https://api.groq.com/v1/request_manager/text_completion"

  t0 <- liftIO Time.getCurrentTime
  resp <- HTTP.getResponseBody <$> HTTP.httpLBS request
  t1 <- liftIO Time.getCurrentTime
  model <- optsModel <$> lift ask

  let results =
        catMaybes <$> fmap (Aeson.decode @(Result Content)) . C8.lines $ resp
      perf =
        catMaybes <$> fmap (Aeson.decode @(Result Stats)) . C8.lines $ resp

      perfLogs = maybe [] ((: []) . showPerf model t0 t1 . stats . result) (head perf)

      final = mconcat (content . result <$> results)

      logLlmCalls logFile = for_
        [ ("s", yellow sysPrompt),
          ("u", blue userPrompt),
          ("r", final)
        ]
        $ \(prefix, content) ->
          appendFile logFile $ indentWith (prefix <> ":") $ content <> "\n"

  lift ask >>= maybe pass (liftIO . logLlmCalls) . optsLogLlm

  lift $ modify $ \case
    Nothing -> Nothing
    Just s@Iter {iterPerfLogs} ->
      Just $ s {iterPerfLogs = iterPerfLogs <> perfLogs}

  pure final

showPerf :: Text -> Time.UTCTime -> Time.UTCTime -> PerformanceStats -> Text
showPerf model t0 t1 PerformanceStats {timeGenerated, tokensGenerated, timeProcessed} =
  T.pack
    $ Printf.printf
      "%0.3fs Tokens/s | %s | total: %ss 💤queued for %ss 👂input: %0.3fs 💬gen: %0.3fs"
      (fromInteger (toInteger tokensGenerated) / timeGenerated)
      model
      (showDiffTime requestTime)
      (showDiffTime queueTime)
      timeProcessed
      timeGenerated
  where
    showDiffTime = Time.formatTime Time.defaultTimeLocale "%3Es"
    queueTime = requestTime - realToFrac timeGenerated - realToFrac timeProcessed
    requestTime = Time.utcTimeToPOSIXSeconds t1 - Time.utcTimeToPOSIXSeconds t0

mkHistorymessages :: [(Text, Text)] -> [HistoryMessage]
mkHistorymessages messages =
  [ HistoryMessage {userPrompt, assistantResponse}
    | (userPrompt, assistantResponse) <- messages
  ]
