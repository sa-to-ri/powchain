{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Routes where

import qualified Web.Scotty.Trans              as ScottyT
import           Network.Wai.Middleware.Static    (static)
import           Network.HTTP.Types.Status        (ok200, mkStatus)

import qualified Data.Aeson                    as Aeson

import qualified Env                           as Env
import           Env                              (Server, ServerAction)
import qualified Node                          as Node
import           Views

routes :: Server ()
routes = do
  ScottyT.middleware static
   
  ScottyT.get    "/"             $ Env.getChain        >>= ScottyT.html . mainView
  ScottyT.get    "/mine"         $ Node.mine
  ScottyT.get    "/chain"        $ Env.getChain        >>= ScottyT.html . chainView
  ScottyT.put    "/chain"        $ putChain 
  ScottyT.get    "/transactions" $ Env.getTransactions >>= ScottyT.html . transactionView
  ScottyT.post   "/transactions" $ postTransaction
  ScottyT.get    "/peers"        $ Node.checkPeers     >>= ScottyT.html . peerView
  ScottyT.post   "/peers"        $ postPeer
  ScottyT.delete "/peers/:peer"  $ deletePeer
  ScottyT.get    "/heartbeat"    $ ScottyT.status ok200
  where
    putChain :: ServerAction ()
    putChain = Aeson.decode <$> ScottyT.body >>= \case
      Just foreignChain -> Node.updateChain foreignChain
      Nothing           -> ScottyT.status $ mkStatus 400 "Chain could not be parsed."

    postTransaction :: ServerAction ()
    postTransaction = Aeson.decode <$> ScottyT.body >>= \case
      Just transaction -> Node.addTransaction transaction
      Nothing          -> ScottyT.status $ mkStatus 400 "Transaction could not be parsed."

    postPeer :: ServerAction ()
    postPeer = Aeson.decode <$> ScottyT.body >>= \case
      Just peer -> Node.addPeer peer
      Nothing   -> ScottyT.status $ mkStatus 400 "Peer could not be parsed."

    deletePeer :: ServerAction ()
    deletePeer = ScottyT.param "peer" >>= Env.deletePeer

