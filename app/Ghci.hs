module Ghci (demo) where

import qualified Data.Yaml as Yaml
import Main (Opts (..), defaultOpts, iterWithOpts)
import Protolude

demo :: IO ()
demo = do
  opts <- Yaml.decodeFileThrow "demo/demo.yml"
  iterWithOpts opts "demo/demo.py"

self :: IO ()
self = do
  opts <- Yaml.decodeFileThrow "demo/hs.yml"
  iterWithOpts opts "app/Main.hs"
