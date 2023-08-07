{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Test.Hspec               (Spec, hspec, before_, describe, context, it, shouldBe, shouldSatisfy)
import           Test.Hspec.Wai           (with, shouldRespondWith, post, put)

import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (run)

import           Web.Scotty.Trans         (scottyAppT)

import qualified Data.Aeson as Aeson
import qualified Data.Set as Set
import           Data.Foldable            (traverse_)
import           Data.Either              (isRight, isLeft)

import           Control.Concurrent       (forkIO)
import           Control.Monad            (void)
import           Control.Monad.Reader     (runReaderT)

import           Routes                   (routes)
import           Env                      (initNodeState)

import           Block
import           Chain    
import           Ledger
import           Transaction

endpoint :: IO Application
endpoint = do
  nodeState <- initNodeState "localhost" "3000"
  scottyAppT (`runReaderT` nodeState) routes

main :: IO ()
main = traverse_ hspec [blockSpec, chainSpec, ledgerSpec, restSpec]

blockSpec :: Spec
blockSpec = 
  describe "Block" $ do
    describe "hasValidProof" $ do
      it "returns False when provided with a block without a valid proof" $ do
        hasValidProof emptyBlock `shouldBe` False
      it "returns True when provided with a block with a valid proof" $ do
        hasValidProof genesis `shouldBe` True
    
    describe "generateProof" $ do
      it "generates a valid proof for a given block" $ do
        (hasValidProof $ generateProof emptyBlock) `shouldBe` True 

chainSpec :: Spec
chainSpec =
  describe "Chain" $ do
    describe "isValidChain" $ do
      context "When provided with a chain that contains blocks with invalid proofs" $ do
        it "returns False" $ do
          isValidChain invalidChain1 `shouldBe` False
      
      context "When provided with a chain that contains blocks out of order" $ do
        it "returns False" $ do
          isValidChain invalidChain2 `shouldBe` False
      
      context "When provided with a valid chain" $ do
        it "returns True " $ do
          isValidChain validChain `shouldBe` True
     
    describe "resolveConsensus" $ do
      context "When provided with an invalid foreign chain that is longer than the local" $ do
        it "returns an error" $ do
          resolveConsensus emptyLedger validChain invalidChain3 `shouldSatisfy` isLeft

      context "When provided with a valid foreign chain that is shorter than the local" $ do
        it "returns an error" $ do
          resolveConsensus emptyLedger validChain emptyChain `shouldSatisfy` isLeft
      
      context "When provided with a valid foreign chain that is as long as the local" $ do
        it "returns an error" $ do
          resolveConsensus emptyLedger validChain validChain `shouldSatisfy` isLeft

      context "When provided with a longer, valid foreign chain that contains invalid transactions" $ do
        it "returns an error" $ do
          resolveConsensus emptyLedger emptyChain invalidChain4 `shouldSatisfy` isLeft

      context "When provided with a valid foreign chain that is longer than the local" $ do
        it "returns a ledger corresponding to the foreign chain" $ do
          resolveConsensus emptyLedger emptyChain validChain `shouldSatisfy` isRight
  where
    invalidTx     = mkTransaction_ (read "1s") "aaa" "bbb" 1000
    invalidChain1 = addBlock emptyBlock { previousBlock = hashBlock genesis } emptyChain
    invalidChain2 = addBlock (generateProof emptyBlock) emptyChain
    invalidChain3 = addBlock emptyBlock . addBlock emptyBlock $ emptyChain
    invalidChain4 = addBlock (generateProof emptyBlock { previousBlock = hashBlock genesis, transactions = Set.singleton invalidTx }) emptyChain
    validChain    = addBlock (generateProof emptyBlock { previousBlock = hashBlock genesis }) emptyChain

ledgerSpec :: Spec
ledgerSpec =
  describe "Ledger" $ do
    describe "addTransaction" $ do
      context "When provided with a transaction where the sender lacks funds" $ do
        it "returns an error" $ do
          addTransaction tx3 emptyLedger `shouldSatisfy` isLeft

      context "When provided with a duplicate transaction" $ do
        it "returns an error" $ do
          (addTransaction tx1 emptyLedger >>= addTransaction tx1) `shouldSatisfy` isLeft

      context "When provided with a valid transaction" $ do
        it "returns an updated ledger" $ do
          addTransaction tx1 emptyLedger `shouldSatisfy` isRight

    describe "addTransactions" $ do
      context "When provided with a set containing invalid transactions" $ do
        it "returns an error" $ do
          addTransactions invalidTxs emptyLedger `shouldSatisfy` isLeft

      context "When provided with a set containing only valid transactions" $ do
        it "returns an updated ledger" $ do
          addTransactions validTxs emptyLedger `shouldSatisfy` isRight

    describe "isValid" $ do
      context "When provided with an invalid transaction" $ do
        it "returns an error" $ do
          isValid emptyLedger tx3 `shouldSatisfy` isLeft
      
      context "When provided with a valid transaction" $ do
        it "returns the transaction" $ do
          isValid emptyLedger tx1 `shouldSatisfy` isRight

    describe "filterValid" $ do
      context "When provided with a set containing invalid transactions" $ do
        it "returns the set with all invalid transactions removed" $ do
          filterValid emptyLedger invalidTxs `shouldBe` Set.fromList [tx2, tx4]
      
      context "When provided with a set containing only valid transactions" $ do
        it "returns the original set" $ do
          filterValid emptyLedger validTxs `shouldBe` validTxs 
  where
    tx1         = mkTransaction_ (read "1s") "admin" "b" 100
    tx2         = mkTransaction_ (read "2s") "admin" "c" 100
    tx3         = mkTransaction_ (read "3s") "b"     "c" 50
    tx4         = mkTransaction_ (read "4s") "c"     "d" 50
    validTxs    = Set.fromList [tx1, tx2, tx3, tx4]
    invalidTxs  = Set.fromList [tx2, tx3, tx4]

restSpec :: Spec
restSpec = with endpoint $ do
  describe "REST routes" $ do
    describe "put chain" $ do
      context "When provided with an invalid chain" $ do
        it "should respond with 400" $ do
          put "/chain" (Aeson.encode invalidChain) `shouldRespondWith` 400
             
      context "When provided with an equally long chain" $ do
        it "should respond with 400" $ do
          put "/chain" (Aeson.encode emptyChain) `shouldRespondWith` 400
             
      context "When provided with a longer, valid chain" $ do
        it "should respond with 200" $ do
          put "/chain" (Aeson.encode validChain) `shouldRespondWith` 200
             
    describe "post transactions" $ do
      context "When provided with an invalid transaction" $ do
        it "should respond with 400" $ do
          post "/transactions" (Aeson.encode invalidTx) `shouldRespondWith` 400
      
      context "When provided with a valid transaction" $ do
        it "should respond with 201" $ do
          post "/transactions" (Aeson.encode validTx) `shouldRespondWith` 201
             
    describe "post peer" $ do
      context "When provided with an unresponsive peer" $ do
        it "should return 400" $ do
          post "/peers" (Aeson.encode peer) `shouldRespondWith` 400 
      
      before_ (void . forkIO . run 3000 =<< endpoint) $ do
        context "When provided with a responsive peer" $ do
          it "should return 201" $ do
            post "/peers" (Aeson.encode peer) `shouldRespondWith` 201 
  where
    invalidChain = addBlock emptyBlock emptyChain
    validChain   = addBlock (generateProof emptyBlock { previousBlock = hashBlock genesis }) emptyChain
    invalidTx    = mkTransaction_ (read "1s") "b"     "c" 100
    validTx      = mkTransaction_ (read "1s") "admin" "b" 100
    peer         = ("http://localhost:3000" :: String)


      


