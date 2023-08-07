{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Ledger 
  ( Ledger
  , Error
  , emptyLedger
  , getBalance
  , isValid
  , filterValid
  , addTransaction
  , addTransactions
  , processBlock
  , processBlocks
  )
  where

import           Control.Monad      ((>=>))

import           Data.Map           (Map)
import qualified Data.Map        as Map
import           Data.Set           (Set)
import qualified Data.Set        as Set
import           Data.Text.Lazy     (Text)
import           Data.ByteString    (ByteString)

import           Block              (Block)
import qualified Block           as Block
import           Transaction        (Transaction, sender, recipient, amount)

-- | A ledger consists of a map containing current balances for all known accounts and a set of
--   all processed transactions.
data Ledger = Ledger
  { accounts     :: Map Text Int
  , transactions :: Set Transaction }
  deriving (Show, Eq)

type Error = ByteString

emptyLedger :: Ledger
emptyLedger = Ledger { accounts     = Map.insert "admin" 1000 $ Map.empty
                     , transactions = Set.empty }

getBalance :: Text -> Ledger -> Int
getBalance account ledger = Map.findWithDefault 0 account (accounts ledger)

addFunds :: Text -> Int -> Ledger -> Ledger
addFunds account amount ledger = ledger { accounts = Map.insertWith (+) account amount (accounts ledger) }

drawFunds :: Text -> Int -> Ledger -> Ledger
drawFunds account amount ledger = ledger { accounts = Map.insertWith subtract account amount (accounts ledger) }

logTransaction :: Transaction -> Ledger -> Ledger
logTransaction transaction ledger = ledger { transactions = Set.insert transaction (transactions ledger) }

senderOwnsBalance :: Ledger -> Transaction -> Either Error Transaction
senderOwnsBalance ledger transaction
  | getBalance (sender transaction) ledger >= (amount transaction) = Right transaction
  | otherwise                                                      = Left "Insufficient funds."

transactionIsUnique :: Ledger -> Transaction -> Either Error Transaction
transactionIsUnique ledger transaction
  | Set.notMember transaction (transactions ledger) = Right transaction
  | otherwise                                       = Left "Transaction already exists."

-- | Monadic and easily extendible validation function that checks a number of conditions and returns
--   either a validated transaction or an error corresponding to the first failed condition.
isValid :: Ledger -> Transaction -> Either Error Transaction 
isValid ledger = transactionIsUnique ledger >=> senderOwnsBalance ledger

-- | Filters a set of transaction and returns only those transactions that would be valid given the
--   provided ledger.
filterValid :: Ledger -> Set Transaction -> Set Transaction
filterValid ledger transactions = snd $ foldl filterNext (ledger, Set.empty) transactions
  where
    filterNext :: (Ledger, Set Transaction) -> Transaction -> (Ledger, Set Transaction)
    filterNext (ldg, txs) tx = case addTransaction tx ldg of
                         Left _     -> (ldg, txs)
                         Right ldg' -> (ldg', Set.insert tx txs)

-- | Attempts to add a set of transactions to a ledger and returns either an updated ledger containing
--   all provided transactions or an error corresponding to the first failed transaction.
addTransactions :: Set Transaction -> Ledger -> Either Error Ledger
addTransactions transactions ledger = foldl addNext (Right ledger) (Set.toList transactions)
  where
    addNext :: Either Error Ledger -> Transaction -> Either Error Ledger
    addNext ldg tx = ldg >>= addTransaction tx

-- | Attempts to add a single transaction to a ledger and returns either an updated ledger containing
--   the provided transaction or an error.
addTransaction :: Transaction -> Ledger -> Either Error Ledger
addTransaction transaction ledger = isValid ledger transaction >>= add ledger
  where
    add :: Ledger -> Transaction -> Either Error Ledger
    add ldg tx = pure
                  . logTransaction tx
                  . addFunds  (recipient tx) (amount tx) 
                  . drawFunds (sender tx)    (amount tx) 
                  $ ldg

-- | Attempts to add all transactions from a single block to a ledger and returns either an updated ledger 
--   containing all provided transactions or an error corresponding to the first failed transaction.
processBlock :: Block -> Ledger -> Either Error Ledger
processBlock = addTransactions . Block.transactions

-- | Attempts to add all transactions from a list of blocks to a ledger and returns either an updated ledger 
--   containing all provided transactions or an error corresponding to the first failed transaction.
processBlocks :: [Block] -> Ledger -> Either Error Ledger
processBlocks blocks ledger = foldr (\block -> (>>= processBlock block)) (Right ledger) blocks

