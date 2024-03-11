{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main, iterWithOpts, Opts (..), defaultOpts) where

import Common
import Data.Aeson ((.=))
import Data.Aeson as Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson (decode, object)
import qualified Data.Algorithm.Diff as Diff (getGroupedDiff)
import qualified Data.Algorithm.DiffOutput as Diff
  ( diffToLineRanges,
    prettyDiffs,
  )
import qualified Data.List as List (elem, words)
import qualified Data.List.NonEmpty as NE
  ( cons,
    head,
    nonEmpty,
    tail,
  )
import qualified Data.Map as Map (assocs, lookup)
import Data.String (String)
import qualified Data.Text as T (pack, replace, unpack)
import qualified Data.Time.Clock.POSIX as Time (getCurrentTime)
import qualified Data.Yaml as Yaml (decodeFileThrow)
import Legacy
import qualified Network.HTTP.Simple as HTTP
  ( getResponseBody,
    httpLBS,
    setRequestBodyJSON,
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
import qualified System.Console.Haskeline as H
  ( defaultSettings,
    getInputLine,
    handleInterrupt,
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

parserInfo :: OA.ParserInfo (FilePath, Maybe FilePath)
parserInfo =
  OA.info (optionParser <**> OA.helper) $
    OA.fullDesc
      <> OA.header "llm-based iteration tool."

optionParser :: OA.Parser (FilePath, Maybe FilePath)
optionParser = do
  cfg <- OA.optional $ OA.strOption $ OA.long "cfg" <> OA.metavar "<file>"
  content <- OA.strArgument $ OA.metavar "<file>"
  return (content, cfg)

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
iterWithOpts opts path = do
  liftIO (Environment.lookupEnv "GROQ_API_KEY") >>= \case
    Nothing -> case optsToken opts of
      Nothing ->
        (putText . unlines)
          [ "GROQ_API_KEY not set. Run:",
            "",
            "  export HISTIGNORE=$HISTIGNORE':*GROQ_API_KEY*' # optional, bash only",
            "  export GROQ_API_KEY=<your key>",
            "",
            "No key yet? Create an account and generate yours: https://console.groq.com"
          ]
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
    H.getInputLine $
      mconcat
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
    Just _ -> oneProposalStep instructions (T.pack raw) old new >>= proposeMore instructions numIters

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
    Just code -> H.handleInterrupt (outputText "Cancelled.") $
      H.withInterrupt $
        do
          outputText ""
          oneDiscussionStep prompt code >>= (outputText . blue)
          dumpPerformanceLogs (Just "")

styleGuidelines :: [Text]
styleGuidelines =
  [ "Reply in markdown.",
    "Keep all the source code in a single file, don't split things apart in multiple files.",
    "don't add other files"
  ]

oneProposalStep :: Text -> Text -> Text -> Text -> IterT Text
oneProposalStep goal instructions old new = do
  mFilePath <- getFilePath
  llm [] (sysPrompt mFilePath) userPrompt
  where
    userPrompt =
      [ "Our overall task is:" <> goal,
        "Here's the adjustment I am requesting to our diff now:" <> instructions
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
        "Which means our new version of the code is:",
        "```",
        new,
        "```",
        "When asked for a change, return ONLY YOUR NEW VERSION OF THE CODE.",
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
      [ "Here's the current version of the source:",
        "```",
        code,
        "```",
        "And here's the task we're working on:",
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
          [ "You will write a new version of a source file.",
            "Return ONLY YOUR NEW VERSION OF THE CODE and nothing else",
            "Do NOT say anything else.",
            ""
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
        [ "Apply that to the answer. Return ONLY your new version of the",
          " original source code, and nothing else"
        ]
      ]

    sysPrompt mFilePath =
      [ "We are working together on changing code block below:",
        "```",
        code,
        "```",
        "Here the initial diff you've provided. Note: I'm not sure that is an improvement (maybe?).",
        "```",
        diffString code new,
        "```",
        "Our goal is to satisfy the following request:",
        instructions
      ]
        <> styleGuidelines
        <> btwFileNameIs mFilePath

llm :: [(Text, Text)] -> [Text] -> [Text] -> IterT Text
llm historyMessages sysPrompt userPrompt =
  do
    legacy <- lift $ fromMaybe defaultOptsLegacyApi . optsLegacyApi <$> ask
    let f = if legacy then llmLegacy else llmOpenAiApi
    f historyMessages sysPrompt userPrompt

llmOpenAiApi :: [(Text, Text)] -> [Text] -> [Text] -> IterT Text
llmOpenAiApi historyMessages (unlines -> sysPrompt) (unlines -> userPrompt) = do
  token <- optsToken <$> lift ask
  model <- optsModel <$> lift ask
  let body =
        [ "model" .= model,
          "messages"
            .= mconcat
              [ [Message {role = "system", Main.content = sysPrompt}],
                mconcat
                  [ [ Message {role = "user", Main.content = prompt},
                      Message {role = "assistant", Main.content = reply}
                    ]
                    | (prompt, reply) <- historyMessages
                  ],
                [Message {role = "user", Main.content = userPrompt}]
              ]
        ]
      request =
        setBearer token $
          HTTP.setRequestBodyJSON
            (Aeson.object body)
            "POST https://api.groq.com/openai/v1/chat/completions"

  t0 <- liftIO Time.getCurrentTime
  resp <- HTTP.getResponseBody <$> HTTP.httpLBS request
  t1 <- liftIO Time.getCurrentTime

  case Aeson.decode @OpenAiResult resp of
    Just result@OpenAiResult {choices} -> case head choices of
      Just Choice {message} -> do
        pushPerf
          [ showPerf
              model
              t0
              t1
              PerformanceStats
                { timeGenerated = completion_time $ usage result,
                  tokensGenerated = completion_tokens $ usage result,
                  timeProcessed = prompt_time $ usage result,
                  tokensProcessed = prompt_tokens $ usage result
                }
          ]
        pure $ Main.content message
      Nothing -> pure ""
    Nothing -> pure ""

data OpenAiResult = OpenAiResult
  { id :: Text,
    object :: Text,
    created :: Int,
    model :: Text,
    choices :: [Choice],
    usage :: Usage
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON)

data Usage = Usage
  { prompt_time :: Double,
    prompt_tokens :: Int,
    completion_time :: Double,
    completion_tokens :: Int,
    total_time :: Double,
    total_tokens :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data Choice = Choice
  { index :: Int,
    message :: Message,
    logprobs :: ()
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data Message = Message {role :: Text, content :: Text}
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

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
