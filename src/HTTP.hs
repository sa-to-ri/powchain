module HTTP where

import qualified Data.Aeson               as Aeson
import qualified Data.Text.Lazy           as Text
import           Data.ByteString.Lazy        (ByteString)
import           Data.Foldable               (traverse_)

import qualified Control.Concurrent.Async as Async
import qualified Network.Wreq             as Wreq

import           Control.Monad.IO.Class      (liftIO)

import qualified Env                      as Env
import           Env                         (ServerAction)
import           Chain                       (Chain)
import           Transaction                 (Transaction)

type Host = String
type Path = String
type Body = ByteString

data Request 
  = Get    Path
  | Post   Path Body
  | Put    Path Body
  | Delete Path 
  deriving (Show, Eq)

data RequestResult = Success | Failure
  deriving (Eq, Ord)

instance Show RequestResult where
  show Success = "success"
  show Failure = "failure"

-- | Makes an asynchronous request, waits for the result, and returns an indicator of
--   whether the request was successful or not.
asyncRequest :: Request -> Host -> IO RequestResult
asyncRequest request host = do
  Async.withAsync (mkRequest request) $ \ a -> do

    result <- Async.waitCatch a
    case result of
      Left _  -> pure Failure 
      Right _ -> pure Success 
  where
    mkRequest (Get    path)      = Wreq.get    ("http://" <> host <> path)
    mkRequest (Post   path body) = Wreq.post   ("http://" <> host <> path) body
    mkRequest (Put    path body) = Wreq.put    ("http://" <> host <> path) body
    mkRequest (Delete path)      = Wreq.delete ("http://" <> host <> path)

-- | Sends a request to all known peers.
broadcast :: Request -> ServerAction ()
broadcast request = do
  peers <- Env.getPeers
  liftIO $ traverse_ (asyncRequest request . Text.unpack) peers

-- | Sends a request to add a transaction to all known peers.
broadcastTransaction :: Transaction -> ServerAction ()
broadcastTransaction transaction = broadcast $ Post "/transactions" (Aeson.encode transaction)
 
-- | Sends a request to update the chain to all known peers.
broadcastChain :: Chain -> ServerAction ()
broadcastChain chain = broadcast $ Put "/chain" (Aeson.encode chain)

