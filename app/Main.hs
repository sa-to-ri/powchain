module Main (main) where

import Web.Scotty.Trans     (scottyT)

import Control.Monad.Reader (runReaderT)

import Options.Applicative  (Parser, ParserInfo, (<**>), execParser, argument, str, auto, metavar, info, helper)

import Routes               (routes)
import Env                  (initNodeState)

data Args = Args String Int

-- | Parser that attempts to read hostname and port from the provided arguments.
argsParser :: Parser Args
argsParser = Args
  <$> argument str  (metavar "host") 
  <*> argument auto (metavar "port")

-- | Start scotty server with provided routes. ReaderT is used to provide a global environment.
main :: IO ()
main = execParser args >>= \ (Args host port) -> do 
  nodeState <- initNodeState host (show port)
  
  scottyT port (`runReaderT` nodeState) routes
  where
    args :: ParserInfo Args
    args = info (argsParser <**> helper) mempty

