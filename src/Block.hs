{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveGeneric #-}

module Block
  ( Block
  , blockId
  , transactions
  , previousBlock
  , proof
  , emptyBlock
  , genesis
  , hashBlock
  , generateProof
  , isSuccessorTo
  , hasValidProof
  ) where

import           GHC.Generics

import           Crypto.Hash             (hashWith, SHA256 (..), Digest)

import qualified Numeric              as Numeric

import qualified Data.Aeson           as Aeson
import           Data.Aeson
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Set                (Set)

import           Transaction             (Transaction)

data Block = Block 
  { blockId       :: Int
  , transactions  :: Set Transaction
  , previousBlock :: String
  , proof         :: String
  } deriving (Generic, Show, Eq)

instance FromJSON Block
instance ToJSON Block

emptyBlock :: Block
emptyBlock = Block
  { blockId       = 0
  , transactions  = []
  , previousBlock = ""
  , proof         = ""
  }

genesis :: Block
genesis = emptyBlock { proof = "16a08" }

isSuccessorTo :: Block -> Block -> Bool
isSuccessorTo b b' = previousBlock b == hashBlock b'

hash :: LBS.ByteString -> Digest SHA256
hash = hashWith SHA256 . BS.concat . LBS.toChunks

hashBlock :: Block -> String
hashBlock = show . hash . Aeson.encode

digestToInt :: Digest a -> Integer
digestToInt = read . ("0x" <>) . show

target :: Integer
target = 0x0001000000000000000000000000000000000000000000000000000000000000

-- | Checks whether or not a block contains a valid proof of work. A block contains a valid proof of work 
--   if the block hash is smaller than a set target value. In our case, the target value is relatively easy 
--   to beat, but in practice the target value is typically modified so that mining time is kept constant 
--   and reasonably long.
hasValidProof :: Block -> Bool
hasValidProof block = 
  let
    actual = digestToInt . hash $ Aeson.encode block
  in
    target > actual

-- | Generates a proof of work for a provided block by brute force.
generateProof :: Block -> Block
generateProof = go 0
  where
    go :: Integer -> Block -> Block
    go n block
      | hasValidProof block { proof = Numeric.showHex n "" } = block { proof = Numeric.showHex n "" }
      | otherwise                                            = go (n + 1) block

