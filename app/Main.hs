{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main, iterWithOpts, Opts (..), defaultOpts) where

import Control.Arrow ((>>>))
import Data.Aeson ((.=))
import Data.Aeson as Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
  ( Options,
    ToJSON (toEncoding, toJSON),
    decode,
    defaultOptions,
    fieldLabelModifier,
    genericParseJSON,
    genericToEncoding,
    genericToJSON,
    object,
    parseJSON,
  )
import qualified Data.Algorithm.Diff as Diff (getGroupedDiff)
import qualified Data.Algorithm.DiffOutput as Diff
  ( diffToLineRanges,
    prettyDiffs,
  )
import qualified Data.ByteString.Lazy.Char8 as C8 (lines)
import qualified Data.List as List (elem, words)
import qualified Data.List.NonEmpty as NE
  ( cons,
    drop,
    head,
    length,
    nonEmpty,
    tail,
  )
import qualified Data.List.Split as Split (splitWhen)
import qualified Data.Map as Map (assocs, empty, lookup)
import Data.String (String)
import qualified Data.Text as T (isPrefixOf, pack, replace, unpack)
import qualified Data.Time.Clock as Time (UTCTime)
import qualified Data.Time.Clock.POSIX as Time (getCurrentTime, utcTimeToPOSIXSeconds)
import qualified Data.Time.Format as Time (defaultTimeLocale, formatTime)
import qualified Data.Yaml as Yaml (decodeFileThrow)
import qualified Network.HTTP.Simple as HTTP
  ( Request,
    getResponseBody,
    httpLBS,
    setRequestBodyJSON,
    setRequestHeader,
  )
import qualified Options.Applicative as OA
  ( Parser,
    ParserInfo,
    execParser,
    fullDesc,
    header,
    helper,
    info,
    long,
    metavar,
    optional,
    strArgument,
    strOption,
  )
import Protolude
import System.Console.Haskeline (InputT)
import qualified System.Console.Haskeline as H
  ( defaultSettings,
    getInputLine,
    handleInterrupt,
    outputStrLn,
    runInputT,
    withInterrupt,
  )
import qualified System.Environment as Environment (lookupEnv)
import qualified System.FilePath as FilePath (dropExtension, takeFileName)
import System.IO (BufferMode (NoBuffering))
import qualified System.IO as IO
  ( hClose,
    hFlush,
    hPutStr,
    hSetBinaryMode,
    hSetBuffering,
  )
import qualified System.IO.Temp as IO (withSystemTempFile)
import System.Process (ProcessHandle)
import qualified System.Process as Process
  ( readProcessWithExitCode,
    spawnProcess,
    waitForProcess,
  )
import qualified Text.Casing as Casing (fromHumps, toKebab)
import qualified Text.Printf as Printf (printf)

data Opts token = Opts
  { optsIcDiffArgs :: Maybe [Text],
    optsFeedback :: Map Text [Text],
    optsLogLlm :: Maybe FilePath,
    optsModel :: Text,
    optsToken :: token
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

outputText :: Text -> IterT ()
outputText = H.outputStrLn . T.unpack

readProcessWithExitCode :: Text -> [Text] -> Text -> IO (ExitCode, Text, Text)
readProcessWithExitCode
  (T.unpack -> exe)
  (fmap T.unpack -> args)
  (T.unpack -> stdIn) = do
    (ec, out, err) <- Process.readProcessWithExitCode exe args stdIn
    return (ec, T.pack out, T.pack err)

spawnProcess :: Text -> [Text] -> IO ProcessHandle
spawnProcess (T.unpack -> cmd) (fmap T.unpack -> args) =
  Process.spawnProcess cmd args

aesonOpts :: Aeson.Options
aesonOpts =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Casing.toKebab . Casing.fromHumps . drop 4
    }

defaultOpts :: Opts (Maybe Token)
defaultOpts =
  Opts
    { optsIcDiffArgs = Just defaultIcDiffArgs,
      optsFeedback = Map.empty,
      optsLogLlm = Nothing,
      optsModel = "mixtral-8x7b-4096",
      optsToken = Nothing
    }

defaultIcDiffArgs :: [Text]
defaultIcDiffArgs = ["-H", "--label=✖ CURRENT ✖", "--label=✔ NEW ✔", "--cols=160"]

withTokenAndDefaults :: Opts (Maybe Token) -> Token -> Opts Token
withTokenAndDefaults Opts {..} token =
  Opts
    { optsIcDiffArgs = Just $ fromMaybe defaultIcDiffArgs optsIcDiffArgs,
      optsFeedback,
      optsLogLlm,
      optsModel,
      optsToken = token
    }

parserInfo :: OA.ParserInfo (FilePath, Maybe FilePath)
parserInfo =
  OA.info (optionParser <**> OA.helper)
    $ OA.fullDesc
    <> OA.header "llm-based iteration tool."

optionParser :: OA.Parser (FilePath, Maybe FilePath)
optionParser = do
  cfg <- OA.optional $ OA.strOption $ OA.long "cfg" <> OA.metavar "<file>"
  content <- OA.strArgument $ OA.metavar "<file>"
  return (content, cfg)

mapOrComplainAndReturnOld :: (Text -> IterT t) -> IterT (Maybe (Text, t))
mapOrComplainAndReturnOld f =
  lift get >>= \case
    Nothing -> outputText "no content yet" >> pure Nothing
    Just (Iter {iterHistory = old :| _}) -> Just . (old,) <$> f old

undo :: IterT Text
undo =
  lift get >>= \case
    Nothing -> pure ""
    Just s@Iter {iterHistory = iterHistory@(lastProposal :| _)} ->
      case NE.nonEmpty (NE.tail iterHistory) of
        Nothing -> pure ""
        Just historyTail -> do
          lift $ put $ Just s {iterHistory = historyTail}
          return lastProposal

main :: IO ()
main =
  OA.execParser parserInfo >>= \(mFile, mConfig) -> case mConfig of
    Nothing -> iter mFile
    Just cfgPath -> do
      opts <- Yaml.decodeFileThrow cfgPath
      iterWithOpts opts mFile

iter :: FilePath -> IO ()
iter = iterWithOpts defaultOpts

iterWithOpts :: Opts (Maybe Token) -> FilePath -> IO ()
iterWithOpts opts path =
  liftIO (Environment.lookupEnv "GROQ_SECRET_ACCESS_KEY") >>= \case
    Nothing -> case optsToken opts of
      Nothing ->
        putText
          "GROQ_SECRET_ACCESS_KEY not set. Get yours: https://wow.groq.com/"
      Just token -> go token
    Just token -> go (T.pack token)
  where
    go token =
      void
        . deepseq token
        $ runReaderT
          (runStateT (H.runInputT H.defaultSettings start) Nothing)
          (opts `withTokenAndDefaults` token)
    start = open path >> loop

whenJust :: (Applicative f) => Maybe a -> (a -> f ()) -> f ()
whenJust ma fb = maybe pass fb ma

getHistoryHead :: IterT (Maybe Text)
getHistoryHead =
  lift get <&> \case
    Nothing -> Nothing
    Just Iter {iterHistory} -> Just (NE.head iterHistory)

getPrevious :: Int -> IterT (Maybe Text)
getPrevious n =
  lift get <&> \case
    Nothing -> Nothing
    Just Iter {iterHistory} -> case NE.drop n iterHistory of
      [] -> Nothing
      content : _ -> Just content

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

runEditor ::
  Text ->
  Text ->
  Text ->
  IO Text
runEditor editorName templ initialContents =
  IO.withSystemTempFile
    (T.unpack templ)
    ( \iterContentPath hdl -> do
        IO.hSetBinaryMode hdl True
        IO.hSetBuffering hdl NoBuffering
        IO.hPutStr hdl (T.unpack initialContents)
        IO.hClose hdl
        prc <- spawnProcess editorName [T.pack iterContentPath]
        void $ Process.waitForProcess prc
        readFile iterContentPath
    )

dumpPerformanceLogs :: Maybe Text -> IterT ()
dumpPerformanceLogs mText =
  lift get >>= \mIter -> whenJust mIter $ \x@Iter {iterPerfLogs} -> do
    whenJust mText outputText
    for_ iterPerfLogs outputText
    lift $ put $ Just x {iterPerfLogs = []}

proposeMore :: Text -> Int -> Text -> IterT ()
proposeMore instructions i new = do
  Just old <- getPrevious i
  opts <- lift ask
  newContent <- outputCurrentDiffAndReturnBlock opts new old
  dumpPerformanceLogs (Just "")
  lift $ modify ((fmap . fmap) (NE.cons newContent))
  propose instructions (i + 1)

outputCurrentDiffAndReturnBlock :: Opts a -> Text -> Text -> IterT Text
outputCurrentDiffAndReturnBlock opts new old = case extractBlock new of
  Nothing -> do
    outputDiff opts old new
    pure new
  Just (before, block, after) -> do
    outputText $ blue before
    outputDiff opts old block
    outputText $ blue after
    pure block

propose :: Text -> Int -> IterT ()
propose _ 0 = pass
propose instructions numIters = do
  iterContentPath <- getFilePath
  Just new <- getHistoryHead
  currentVersion <- getVersion
  Just old <- getPrevious numIters
  mFilePath <- getFilePath
  opts <- lift ask

  minput <-
    H.getInputLine
      $ mconcat
        [ "[",
          show currentVersion,
          " -> ",
          show (currentVersion + 1),
          "] accept/a discard/d edit/e reflexion/r block/b undo/u xplain/x 🔁 "
        ]
  whenJust minput $ \raw -> case NE.nonEmpty (List.words raw) of
    Nothing -> do
      void $ outputCurrentDiffAndReturnBlock opts new old
      propose instructions numIters
    Just (inp :| [])
      | inp `List.elem` ["accept", "a"] -> do
          maybe pass (write new) iterContentPath
          outputStrLnCurrent new
      | inp `List.elem` ["discard", "d"] -> do
          replicateM_ numIters undo
          void $ mapOrComplainAndReturnOld outputStrLnCurrent
      | inp `List.elem` ["xplain", "x"] -> do
          outputText ""
          explainDiff old new >>= outputText . blue
          dumpPerformanceLogs (Just "")
          propose instructions numIters
      | inp `List.elem` ["edit", "e"] ->
          liftIO (Environment.lookupEnv "EDITOR") >>= \case
            Nothing -> do
              outputText "EDITOR environment variable not set"
            Just (T.pack -> editor) ->
              liftIO
                ( runEditor
                    editor
                    ( maybe
                        "code.txt"
                        (T.pack . FilePath.takeFileName)
                        mFilePath
                    )
                    new
                )
                >>= proposeMore instructions numIters
      | inp `List.elem` ["reflexion", "r"] ->
          thinkMore instructions old new >>= proposeMore instructions numIters
      | inp `List.elem` ["undo", "u"] -> do
          undo
          void $ outputCurrentDiffAndReturnBlock opts new old
          propose instructions (numIters - 1)
    Just _ -> oneProposalStep (T.pack raw) old new >>= proposeMore instructions numIters

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

loop :: IterT ()
loop = H.handleInterrupt loop $ H.withInterrupt $ do
  version <- getVersion
  lift get >>= \case
    Nothing -> do
      minput <- H.getInputLine "new/n <file>, open/o <file>, quit/q 🔁 "
      whenJust minput $ \(List.words -> args) -> case args of
        ("q" : _) -> quit
        ("quit" : _) -> quit
        ["o", iterContentPath] -> open iterContentPath
        ["open", iterContentPath] -> open iterContentPath
        ["n", iterContentPath] -> create iterContentPath >> loop
        ["new", iterContentPath] -> create iterContentPath >> loop
        ("n" : _) -> error "new/n takes exactly one argument" >> loop
        ("new" : _) -> error "new/n takes exactly one argument" >> loop
        ("o" : _) -> error "open/o takes exactly one argument" >> loop
        ("open" : _) -> error "open/o takes exactly one argument" >> loop
        ((':' : _) : _) -> outputText "not a :command" >> loop
        [] -> loop
        something -> intend (unwords $ T.pack <$> something) >> loop
    Just Iter {iterHistory, iterContentPath} -> do
      minput <-
        H.getInputLine
          . mconcat
          $ [ "[",
              show version,
              "] :quit/q, :reload/r :write/w, :explain/x :discuss/d :feedback/f ",
              ":undo/u 🔁 "
            ]
      whenJust minput $ \(List.words -> args) -> case args of
        (":q" : _) -> quit
        (":quit" : _) -> quit
        (":r" : _) -> reload >> loop
        (":reload" : _) -> reload >> loop
        [":w"] -> write (NE.head iterHistory) iterContentPath >> loop
        [":write"] -> write (NE.head iterHistory) iterContentPath >> loop
        (":w" : _) -> error ":write/:w takes no argument" >> loop
        (":write" : _) -> error ":write/:w takes no argument" >> loop
        [":explain"] -> explain >> loop
        [":x"] -> explain >> loop
        [":discuss"] -> error ":discuss/d needs to be followed by a prompt" >> loop
        [":d"] -> error ":discuss/d needs to be followed by a prompt" >> loop
        (":discuss" : xs) -> discuss (unwords $ T.pack <$> xs) >> loop
        (":d" : xs) -> discuss (unwords $ T.pack <$> xs) >> loop
        (":feedback" : feedbackName : _) ->
          feedback iterContentPath (T.pack feedbackName) >> loop
        (":f" : feedbackName : _) ->
          feedback iterContentPath (T.pack feedbackName) >> loop
        (":feedback" : _) -> listFeedbacks iterContentPath >> loop
        (":f" : _) -> listFeedbacks iterContentPath >> loop
        (":undo" : _) -> undo >> loop
        (":u" : _) -> undo >> loop
        ((':' : _) : _) -> outputText "not a :command" >> loop
        [] -> mapOrComplainAndReturnOld outputStrLnCurrent >> loop
        prompt -> intend (unwords $ T.pack <$> prompt) >> loop

listFeedbacks :: FilePath -> IterT ()
listFeedbacks iterContentPath = do
  outputText "\nusage: :feedback <key>, where <key> is one of:\n"
  feedbacks <- optsFeedback <$> lift ask
  for_ (Map.assocs feedbacks) $ \(i, fdb) -> case fdb of
    (cmd : (fmap (template iterContentPath) -> args)) ->
      outputText $ i <> ": " <> cmd <> " " <> unwords args
    _ -> pass
  outputText ""

feedback :: FilePath -> Text -> IterT ()
feedback iterContentPath name = do
  feedbacks <- optsFeedback <$> lift ask
  case Map.lookup name feedbacks of
    Just fdb@(cmd : (fmap (template iterContentPath) -> args)) ->
      liftIO (readProcessWithExitCode cmd args "") >>= \case
        (ec, out, err) -> do
          for_
            ["Feedback from " <> unwords ([cmd] <> args), out, err]
            outputText
          case ec of
            ExitSuccess -> outputText "Feedback successful."
            _ -> do
              outputText "Feedback failed. Applying feedback.."
              intend
                . unlines
                $ [ "Apply the following feedback from the `"
                      <> unwords fdb
                      <> "` command to this code:\n",
                    "```",
                    out,
                    err,
                    "```"
                  ]
    _ -> outputText "No such feedback"

template :: FilePath -> Text -> Text
template iterContentPath = templateFile . templateBasename
  where
    baseName = T.pack . FilePath.dropExtension $ FilePath.takeFileName iterContentPath
    fileName = T.pack iterContentPath
    templateFile = T.replace "{file}" fileName
    templateBasename = T.replace "{basename}" baseName

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

quit :: IterT ()
quit = return ()

open :: FilePath -> IterT ()
open iterContentPath = do
  content <- liftIO $ readFile iterContentPath
  lift
    . put
    $ Just
      Iter
        { iterHistory = pure content,
          iterContentPath = iterContentPath,
          iterPerfLogs = []
        }
  void $ mapOrComplainAndReturnOld outputStrLnCurrent

write :: Text -> FilePath -> IterT ()
write content fn = do
  liftIO $ writeFile fn content
  outputText $ "successfully written to " <> T.pack fn

reload :: IterT ()
reload =
  getFilePath >>= \case
    Nothing -> pure ()
    Just fp -> do
      content <- liftIO $ readFile fp
      lift
        . put
        $ Just
          Iter
            { iterHistory = pure content,
              iterContentPath = fp,
              iterPerfLogs = []
            }
      outputText $ "successfully reloaded " <> T.pack fp
      outputStrLnCurrent content

create :: FilePath -> IterT ()
create iterContentPath = do
  liftIO $ writeFile iterContentPath ""
  lift $ put (Just Iter {iterHistory = pure "", iterContentPath, iterPerfLogs = []})

intend :: Text -> IterT ()
intend instructions = do
  H.withInterrupt (H.handleInterrupt cancelIntention go) >>= \case
    Nothing -> pure ()
    Just (old, new)
      | old == new -> outputText "WARNING: no changes" >> outputText ""
      | otherwise -> proposeMore instructions 0 new
  where
    go = mapOrComplainAndReturnOld (oneInstructionStep instructions)
    cancelIntention = outputText "Cancelled" >> pure Nothing

explain :: IterT ()
explain = discuss "Explain what this program does."

discuss :: Text -> IterT ()
discuss prompt = do
  getHistoryHead >>= \case
    Nothing -> return ()
    Just content -> H.handleInterrupt (outputText "Cancelled.")
      $ H.withInterrupt
      $ do
        outputText ""
        oneDiscussionStep prompt content >>= (outputText . blue)
        dumpPerformanceLogs (Just "")

setBearer :: Text -> HTTP.Request -> HTTP.Request
setBearer token =
  HTTP.setRequestHeader "Authorization" ["Bearer " <> encodeUtf8 token]

styleGuidelines :: [Text]
styleGuidelines = ["Reply in markdown."]

oneProposalStep :: Text -> Text -> Text -> IterT Text
oneProposalStep instructions old new = do
  mFilePath <- getFilePath
  llm [] (sysPrompt mFilePath) userPrompt
  where
    userPrompt =
      [ "Here's your task:",
        instructions,
        "Your reply should include a single code block."
      ]

    sysPrompt mFilePath =
      [ "We are working together on the code block below:",
        "```",
        old,
        "```",
        "Here is our diff so far:",
        "```",
        diffString old new,
        "```",
        "I will give you instructions for what to change next.",
        "Return ONLY YOUR NEW VERSION OF THE CODE.",
        "Do NOT say anything else."
      ]
        <> styleGuidelines
        <> btwFileNameIs mFilePath

oneInstructionStep :: Text -> Text -> IterT Text
oneInstructionStep instructions code = do
  mFilePath <- getFilePath
  llm [] (sysPrompt mFilePath) userPrompt
  where
    userPrompt =
      [ "Here's the task:",
        instructions,
        "Here's the current version of the source:",
        "```",
        code,
        "```",
        "Remember, here's the task:",
        instructions,
        "Your reply should include a SINGLE code block. Only one!"
      ]

    sysPrompt mFilePath =
      (if code /= "" then askForNewVersion else askForGenericHelp)
        <> styleGuidelines
        <> btwFileNameIs mFilePath
      where
        askForGenericHelp = ["You help people write software."]
        askForNewVersion =
          [ "You will make a new version of a source file.",
            "Return ONLY YOUR NEW VERSION OF THE CODE and nothing else",
            "Do NOT say anything else."
          ]

oneDiscussionStep :: Text -> Text -> IterT Text
oneDiscussionStep prompt code = do
  mFilePath <- getFilePath
  llm [] (sysPrompt mFilePath) userPrompt
  where
    sysPrompt mFilePath =
      ["You will answer a question about a piece of code."]
        <> styleGuidelines
        <> btwFileNameIs mFilePath
    userPrompt =
      [ "The question is: " <> prompt,
        " It's about this code:",
        "```",
        code,
        "```",
        "Remember, the question is: " <> prompt,
        "Answer in TWO sentences and 50 words MAXIMUM."
      ]

explainDiff :: Text -> Text -> IterT Text
explainDiff = discussDiff "Your task is to describe these changes."

discussDiff :: Text -> Text -> Text -> IterT Text
discussDiff instructions old new = do
  mFilePath <- getFilePath
  llm [] (sysPrompt mFilePath) [instructions]
  where
    sysPrompt mFilePath =
      [ "We are working together on the code block below:",
        "```",
        old,
        "```",
        "Here is our diff so far:",
        "```",
        diffString old new,
        "```"
      ]
        <> styleGuidelines
        <> btwFileNameIs mFilePath

btwFileNameIs :: Maybe FilePath -> [Text]
btwFileNameIs = maybe [] (btw . T.pack)
  where
    btw iterContentPath = ["The file is named " <> iterContentPath <> ", in case that helps."]

thinkMore :: Text -> Text -> Text -> IterT Text
thinkMore instructions code new = do
  mFilePath <- getFilePath
  let go [] history = pure history
      go (prompt : remainingPrompts) history = do
        outputText $ "Self-reflection in progress: " <> blue prompt
        result <- llm history (sysPrompt mFilePath) [prompt]
        dumpPerformanceLogs Nothing
        go remainingPrompts ((prompt, result) : history)
  results <- go (mconcat <$> selfReflexionRecipe) []
  pure $ maybe "" snd (head results)
  where
    -- My boss's [..] boss's boss's favourite self-reflexion prompt.
    selfReflexionRecipe =
      [ [ "Write a detailed outline to answer the following request in a",
          "non-obvious, creative and engaging manner:",
          instructions
        ],
        ["How could this outline be better?"],
        ["Apply that to the outline."],
        ["Now write a response to the request:", instructions],
        ["How could that answer be better?"],
        [ "Apply that to the answer. Return ONLY a new version of the source ",
          "code, and nothing else"
        ]
      ]

    sysPrompt mFilePath =
      [ "We are working together on changing code block below:",
        "```",
        code,
        "```",
        "Here the initial diff we're working from:",
        "```",
        diffString code new,
        "```",
        "and the goal is to satisfy the following request:",
        instructions
      ]
        <> styleGuidelines
        <> btwFileNameIs mFilePath

-- TODO: Move away from request_manager, to the OpenAI API.
llm :: [(Text, Text)] -> [Text] -> [Text] -> IterT Text
llm historyMessages (unlines -> sysPrompt) (unlines -> userPrompt) = do
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

  let results =
        catMaybes <$> fmap (Aeson.decode @(Result Content)) . C8.lines $ resp
      perf =
        catMaybes <$> fmap (Aeson.decode @(Result Stats)) . C8.lines $ resp

      perfLogs = maybe [] ((: []) . showPerf t0 t1 . stats . result) (head perf)

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

mkHistorymessages :: [(Text, Text)] -> [HistoryMessage]
mkHistorymessages messages =
  [ HistoryMessage {userPrompt, assistantResponse}
    | (userPrompt, assistantResponse) <- messages
  ]

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

showPerf :: Time.UTCTime -> Time.UTCTime -> PerformanceStats -> Text
showPerf t0 t1 PerformanceStats {timeGenerated, tokensGenerated, timeProcessed} =
  T.pack
    $ Printf.printf
      "%0.3fs Tokens/s | total: %ss 💤queued for %ss 👂input: %0.3fs 💬gen: %0.3fs"
      (fromInteger (toInteger tokensGenerated) / timeGenerated)
      (showDiffTime requestTime)
      (showDiffTime queueTime)
      timeProcessed
      timeGenerated
  where
    showDiffTime = Time.formatTime Time.defaultTimeLocale "%3Es"
    queueTime = requestTime - realToFrac timeGenerated - realToFrac timeProcessed
    requestTime = Time.utcTimeToPOSIXSeconds t1 - Time.utcTimeToPOSIXSeconds t0

outputDiff :: Opts a -> Text -> Text -> IterT ()
outputDiff Opts {optsIcDiffArgs} old new
  | old == new = outputText "<NO CODE CHANGES>"
  | otherwise =
      liftIO (wrapDiff "icdiff" (fromMaybe [] optsIcDiffArgs) old new) >>= \case
        (ExitSuccess, out, _) -> outputText out
        (ExitFailure i, _, _) ->
          outputText $ "(icdiff failed with exit code" <> show i <> ")"

diffString :: Text -> Text -> Text
diffString (unpackLines -> old) (unpackLines -> new) =
  show . Diff.prettyDiffs . Diff.diffToLineRanges $ Diff.getGroupedDiff old new

unpackLines :: Text -> [String]
unpackLines = fmap T.unpack . lines

wrapDiff :: Text -> [Text] -> Text -> Text -> IO (ExitCode, Text, Text)
wrapDiff cmd args old new =
  IO.withSystemTempFile "iter-old.txt" $ \fileOld hOld -> do
    void $ hPutStrLn hOld old
    IO.hFlush hOld
    IO.withSystemTempFile "iter-new.txt" $ \fileNew hNew -> do
      void $ hPutStrLn hNew new
      IO.hFlush hNew
      readProcessWithExitCode
        cmd
        ([T.pack fileOld, T.pack fileNew] <> args)
        ""
