{-# LANGUAGE PackageImports #-}

import Control.Monad (void)
import qualified System.Environment as Environment
import System.Exit
import Data.List

import qualified Data.HashMap.Lazy as HashMap
import Data.HashMap.Lazy (HashMap)
import qualified "threepenny-gui" Graphics.UI.Threepenny as UI
import "threepenny-gui" Graphics.UI.Threepenny.Core
import qualified "threepenny-gui" Graphics.UI.Threepenny.Elements as Elements
import qualified Data.Yaml as Yaml

usage :: IO ()
usage = do
  pname <- Environment.getProgName
  putStrLn $ "usage: " ++ pname ++ " INPUT"

usageExit :: String -> IO a
usageExit s = do
  putStrLn s
  usage
  exitFailure

inputTrace :: [String] -> IO (HashMap String Yaml.Value)
inputTrace []       = usageExit "expected input file"
inputTrace (a:args) = do
  m_map <- Yaml.decodeFile a
  case m_map of
    (Just map) -> return map
    _          -> usageExit "failed to parse yaml input file"

main :: IO ()
main = startGUI defaultConfig { tpPort = 10000 } setup

setup :: Window -> IO ()
setup window = void $ do
  args <- Environment.getArgs
  trace <- inputTrace args

  return window # set title "mozz-trace"

  getBody window #+ [
    column [
      Elements.h1 # set text "mozz-trace",
      string "hello world",
      string $ show trace
      ]
    ]
