{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Common where

import Control.Arrow ((>>>))
import Data.Aeson as Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
  ( Options,
    ToJSON (toEncoding, toJSON),
    defaultOptions,
    fieldLabelModifier,
    genericParseJSON,
    genericToEncoding,
    genericToJSON,
    parseJSON,
  )
import qualified Data.List.NonEmpty as NE
  ( drop,
    head,
    length,
  )
import qualified Data.List.Split as Split (splitWhen)
import qualified Data.Map as Map (empty)
import qualified Data.Text as T (isPrefixOf, pack, unpack)
import qualified Data.Time.Clock as Time (UTCTime)
import qualified Data.Time.Clock.POSIX as Time (utcTimeToPOSIXSeconds)
import qualified Data.Time.Format as Time (defaultTimeLocale, formatTime)
import qualified Network.HTTP.Simple as HTTP
  ( Request,
    setRequestHeader,
  )
import Protolude
import System.Console.Haskeline (InputT)
import qualified System.Console.Haskeline as H (outputStrLn)
import qualified Text.Casing as Casing (fromHumps, toKebab)
import qualified Text.Printf as Printf (printf)

data Opts token = Opts
  { optsIcDiffArgs :: Maybe [Text],
    optsFeedback :: Map Text [Text],
    optsLogLlm :: Maybe FilePath,
    optsModel :: Text,
    optsToken :: token,
    optsLegacyApi :: Maybe Bool
  }
  deriving stock (Show, Generic)

instance FromJSON (Opts (Maybe Token)) where
  parseJSON = Aeson.genericParseJSON aesonOpts

instance ToJSON (Opts (Maybe Token)) where
  toJSON = Aeson.genericToJSON aesonOpts
  toEncoding = Aeson.genericToEncoding aesonOpts

type Token = Text

type IterT a =
  InputT
    (StateT (Maybe (Iter (NonEmpty Text))) (ReaderT (Opts Token) IO))
    a

data Iter st = Iter
  { iterHistory :: st,
    iterContentPath :: FilePath,
    iterPerfLogs :: [Text]
  }
  deriving stock (Show, Functor)

data PerformanceStats = PerformanceStats
  { timeGenerated :: Double,
    tokensGenerated :: Int,
    timeProcessed :: Double,
    tokensProcessed :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON)

outputText :: Text -> IterT ()
outputText = H.outputStrLn . T.unpack

aesonOpts :: Aeson.Options
aesonOpts =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Casing.toKebab . Casing.fromHumps . drop 4
    }

defaultOpts :: Opts (Maybe Token)
defaultOpts =
  Opts
    { optsIcDiffArgs = Just defaultOptsIcDiffArgs,
      optsFeedback = Map.empty,
      optsLogLlm = Nothing,
      optsModel = "mixtral-8x7b-4096",
      optsToken = Nothing,
      optsLegacyApi = Just defaultOptsLegacyApi
    }

defaultOptsIcDiffArgs :: [Text]
defaultOptsIcDiffArgs = ["-H", "--label=✖ CURRENT ✖", "--label=✔ NEW ✔", "--cols=160"]

defaultOptsLegacyApi :: Bool
defaultOptsLegacyApi = False

withTokenAndDefaults :: Opts (Maybe Token) -> Token -> Opts Token
withTokenAndDefaults Opts {..} token =
  Opts
    { optsIcDiffArgs = Just $ fromMaybe defaultOptsIcDiffArgs optsIcDiffArgs,
      optsFeedback,
      optsLogLlm,
      optsModel,
      optsToken = token,
      optsLegacyApi = Just $ fromMaybe defaultOptsLegacyApi optsLegacyApi
    }

mapOrComplainAndReturnOld :: (Text -> IterT t) -> IterT (Maybe (Text, t))
mapOrComplainAndReturnOld f =
  lift get >>= \case
    Nothing -> outputText "no content yet" >> pure Nothing
    Just (Iter {iterHistory = old :| _}) -> Just . (old,) <$> f old

whenJust :: (Applicative f) => Maybe a -> (a -> f ()) -> f ()
whenJust ma fb = maybe pass fb ma

getHistoryHead :: IterT (Maybe Text)
getHistoryHead =
  lift get <&> \case
    Nothing -> Nothing
    Just Iter {iterHistory} -> Just (NE.head iterHistory)

getVersion :: IterT Int
getVersion =
  lift get <&> \case
    Nothing -> 0
    Just Iter {iterHistory} -> NE.length iterHistory

getFilePath :: IterT (Maybe FilePath)
getFilePath =
  lift get <&> \case
    Nothing -> Nothing
    Just Iter {iterContentPath} -> Just iterContentPath

getPrevious :: Int -> IterT (Maybe Text)
getPrevious n =
  lift get <&> \case
    Nothing -> Nothing
    Just Iter {iterHistory} -> case NE.drop n iterHistory of
      [] -> Nothing
      content : _ -> Just content

dumpPerformanceLogs :: Maybe Text -> IterT ()
dumpPerformanceLogs mText =
  lift get >>= \mIter -> whenJust mIter $ \x@Iter {iterPerfLogs} -> do
    whenJust mText outputText
    for_ iterPerfLogs outputText
    lift $ put $ Just x {iterPerfLogs = []}

extractBlock :: Text -> Maybe (Text, Text, Text)
extractBlock =
  (Split.splitWhen ("```" `T.isPrefixOf`) . lines) >>> \case
    preamble : block : rest ->
      Just
        ( unlines preamble,
          unlines block,
          unlines (mconcat rest)
        )
    _ -> Nothing

error :: Text -> IterT ()
error str = outputText ("error: " <> str)

outputStrLnCurrent :: Text -> IterT ()
outputStrLnCurrent str = do
  outputText ""
  outputText . yellow $ indentWith "  " str

yellow :: Text -> Text
yellow str = "\ESC[93m" <> str <> "\ESC[0m"

blue :: Text -> Text
blue str = "\ESC[34m" <> str <> "\ESC[0m"

indentWith :: Text -> Text -> Text
indentWith c = unlines . map (c <>) . lines

setBearer :: Text -> HTTP.Request -> HTTP.Request
setBearer token =
  HTTP.setRequestHeader "Authorization" ["Bearer " <> encodeUtf8 token]

showPerf :: Text -> Time.UTCTime -> Time.UTCTime -> PerformanceStats -> Text
showPerf model t0 t1 PerformanceStats {timeGenerated, tokensGenerated, timeProcessed} =
  T.pack $
    Printf.printf
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

pushPerf :: [Text] -> IterT ()
pushPerf perfLogs =
  lift $ modify $ \case
    Nothing -> Nothing
    Just s@Iter {iterPerfLogs} ->
      Just $ s {iterPerfLogs = iterPerfLogs <> perfLogs}
