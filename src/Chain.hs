{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Chain 
  ( Chain
  , emptyChain
  , chainLength
  , getLastBlock
  , getBlocks
  , addBlock
  , isValidChain
  , resolveConsensus
  )
  where

import           GHC.Generics

import           Data.Aeson
import           Data.List             (stripPrefix)
import           Data.List.NonEmpty    (NonEmpty, (<|))
import qualified Data.List.NonEmpty as NEList

import           Ledger                (Ledger, Error, emptyLedger)
import qualified Ledger             as Ledger
import           Block                 (Block, isSuccessorTo)
import qualified Block              as Block

newtype Chain = Chain (NonEmpty Block)
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

emptyChain :: Chain
emptyChain = Chain [Block.genesis]

chainLength :: Chain -> Int
chainLength (Chain blocks) = NEList.length blocks

longerThan :: Chain -> Chain -> Bool
longerThan c c' = chainLength c > chainLength c'

getLastBlock :: Chain -> Block
getLastBlock (Chain blocks) = NEList.head blocks

getBlocks :: Chain -> [Block]
getBlocks (Chain blocks) = NEList.toList blocks

addBlock :: Block -> Chain -> Chain
addBlock block (Chain blocks) = Chain $ block <| blocks

-- | Checks that all blocks in a chain are valid. A valid block contains the hash of the previous block
--   and a valid proof.
isValidChain :: Chain -> Bool
isValidChain (Chain blocks) = and $ zipWith validateNext (NEList.toList blocks) (NEList.tail blocks)
  where
    validateNext b b' = Block.hasValidProof b && b `isSuccessorTo` b'

-- | If the second chain is a continuation of the first chain, this function returns only the additional
--   blocks. Otherwise Nothing is returned.
stripPrefixChain :: Chain -> Chain -> Maybe [Block]
stripPrefixChain (Chain bs) (Chain bs') = stripPrefix (NEList.toList bs) (NEList.toList bs')

-- | Checks whether the new chain is valid and longer than the local chain. If so, we attempt to process
--   it. The function either returns a new ledger (meaning that the new chain is valid, longer and only
--   contains valid transactions) or an error corresponding to the specific type of failure condition.
resolveConsensus :: Ledger -> Chain -> Chain -> Either Error Ledger
resolveConsensus ledger lc fc
  | not $ fc `longerThan` lc = Left "Foreign chain is not longer than local."
  | not $ isValidChain fc    = Left "Foreign chain contains invalid blocks."
  | otherwise                = case stripPrefixChain lc fc of
                                 Just blocks -> Ledger.processBlocks blocks ledger
                                 Nothing     -> Ledger.processBlocks (getBlocks fc) emptyLedger

