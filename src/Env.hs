{-# Language OverloadedStrings #-}

module Env 
  ( Server
  , ServerAction
  , NodeState
  , initNodeState
  , getNodeAddress
  , getChain
  , getLedger
  , getTransactions
  , getPeers
  , setChain
  , setLedger
  , modifyTransactions
  , addTransaction
  , addPeer
  , deletePeer
  ) where

import           Web.Scotty.Trans          (ScottyT, ActionT)

import           Control.Monad.Reader      (ReaderT)
import qualified Control.Monad.Reader   as ReaderT
import           Control.Concurrent        (MVar, newMVar, readMVar, modifyMVar_)
import           Control.Monad.IO.Class    (liftIO) 

import           Data.Text.Lazy            (Text)
import qualified Data.Text.Lazy         as Text

import           Data.Set                  (Set)
import qualified Data.Set               as Set

import           Chain
import           Transaction
import           Ledger                    (Ledger, emptyLedger)

type Server       = ScottyT Text (ReaderT NodeState IO)
type ServerAction = ActionT Text (ReaderT NodeState IO)

data NodeState = NodeState
  { address      :: MVar Text
  , chain        :: MVar Chain
  , ledger       :: MVar Ledger
  , transactions :: MVar (Set Transaction)
  , peers        :: MVar (Set Text)
  }

-- | Initiates a state that is accessible from multiple threads using MVars. STM or acid-state
--   would be more powerful, but the additional complexity is not really required.
initNodeState :: String -> String -> IO NodeState
initNodeState host port = do
  address      <- newMVar $ Text.pack (host <> ":" <> port)
  chain        <- newMVar emptyChain 
  ledger       <- newMVar emptyLedger 
  transactions <- newMVar Set.empty
  peers        <- newMVar Set.empty
  
  pure $ NodeState
    { address      = address
    , chain        = chain
    , ledger       = ledger
    , transactions = transactions
    , peers        = peers
    }

-- | We use ReaderT in order to abstract away having to pass the state around.
readState :: ServerAction NodeState
readState = ReaderT.ask

-- | Various functions to read different parts of the state and return it.
readNodeState :: (NodeState -> MVar a) -> ServerAction a
readNodeState field = readState >>= \nodeState ->
  liftIO $ readMVar (field nodeState)
  
getNodeAddress :: ServerAction Text 
getNodeAddress = readNodeState address

getChain :: ServerAction Chain
getChain = readNodeState chain

getLedger :: ServerAction Ledger
getLedger = readNodeState ledger

getTransactions :: ServerAction (Set Transaction)
getTransactions = readNodeState transactions

getPeers :: ServerAction (Set Text)
getPeers = readNodeState peers

-- | Various functions to modify, set, add to or delete from different parts of the state.
modifyNodeState :: (NodeState -> MVar a) -> (a -> a) -> ServerAction ()
modifyNodeState field f = readState >>= \nodeState -> 
  liftIO $ modifyMVar_ (field nodeState) (pure . f)

modifyChain :: (Chain -> Chain) -> ServerAction ()
modifyChain = modifyNodeState chain

setChain :: Chain -> ServerAction ()
setChain = modifyChain . const

modifyLedger :: (Ledger -> Ledger) -> ServerAction ()
modifyLedger = modifyNodeState ledger

setLedger :: Ledger -> ServerAction ()
setLedger = modifyLedger . const

modifyTransactions :: (Set Transaction -> Set Transaction) -> ServerAction ()
modifyTransactions = modifyNodeState transactions

addTransaction :: Transaction -> ServerAction ()
addTransaction = modifyTransactions . Set.insert

modifyPeers :: (Set Text -> Set Text) -> ServerAction ()
modifyPeers = modifyNodeState peers

addPeer :: Text -> ServerAction ()
addPeer = modifyPeers . Set.insert

deletePeer :: Text -> ServerAction ()
deletePeer = modifyPeers . Set.delete

