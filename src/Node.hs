{-# LANGUAGE OverloadedStrings #-}

module Node where

import qualified Web.Scotty.Trans          as ScottyT
import           Network.HTTP.Types.Status    (ok200, created201, mkStatus)

import qualified Data.Set                  as Set
import           Data.Set                     (Set)
import           Data.Text.Lazy               (Text)

import qualified HTTP                      as HTTP
import           HTTP                         (RequestResult(..))
import qualified Env                       as Env  
import           Env                          (ServerAction)  
import qualified Ledger                    as Ledger
import           Chain
import           Block
import           Transaction

-- | Starts mining a new block containing the unprocessed transactions available when mining begins.
--   When mining finishes, the node attempts to replace the local chain.
mine :: ServerAction ()
mine = do
  chain        <- Env.getChain
  transactions <- Env.getTransactions
  ledger       <- Env.getLedger
      
  let nextBlock = generateProof $ emptyBlock { blockId       = chainLength chain
                                             , previousBlock = hashBlock . getLastBlock $ chain
                                             , transactions  = Ledger.filterValid ledger transactions
                                             }

  updateChain $ addBlock nextBlock chain
    
-- | Attempts to replace the local chain with a new chain. Fails if the new chain is invalid
--   or if it is not longer than the local chain. If replacment succeeds, the new chain is
--   broadcasted to all peers.
updateChain :: Chain -> ServerAction ()
updateChain foreignChain = do 
  localChain <- Env.getChain
  ledger     <- Env.getLedger
       
  case resolveConsensus ledger localChain foreignChain of
    Left err        -> ScottyT.status $ mkStatus 400 err
    Right newLedger -> do
      Env.setChain foreignChain
      Env.setLedger newLedger
      Env.modifyTransactions (Ledger.filterValid newLedger)

      HTTP.broadcastChain foreignChain
      ScottyT.status ok200

-- | Attempts to add a transaction to the local pool of unprocessed transactions. Fails if
--   the transaction is invalid, given the current ledger, or if it already exists in the 
--   local pool.
addTransaction :: Transaction -> ServerAction ()
addTransaction transaction = do
  transactions <- Env.getTransactions
  ledger       <- Env.getLedger

  case Ledger.addTransactions transactions ledger >>= Ledger.addTransaction transaction of
    Left err -> ScottyT.status $ mkStatus 400 err
    Right _  -> do
      Env.addTransaction transaction
      HTTP.broadcastTransaction transaction
      ScottyT.status created201  

-- | Adds a new peer to the set of known peers and attempts to register with the peer. Fails, and 
--   removes the peer again, if the peer does not respond.
addPeer :: Text -> ServerAction ()
addPeer peer = do  
  peers <- Env.getPeers 
  
  if (Set.member peer peers)
    then ScottyT.status ok200
    else do
      Env.addPeer peer

      response <- HTTP.registerAt peer
      case response of
        Failure -> do
          Env.deletePeer peer
          ScottyT.status $ mkStatus 400 "Peer could not be reached"
        Success -> ScottyT.status created201

-- | Sends a request to every known peer and returns a set of tuples, indicating whether each
--   peer is available or not.
checkPeers :: ServerAction (Set (Text, RequestResult))
checkPeers = do
  peers   <- Set.toList <$> Env.getPeers
  checked <- traverse HTTP.checkHealth peers
  
  pure $ Set.fromList (zip peers checked)

