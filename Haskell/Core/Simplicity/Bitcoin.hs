-- | This module defines the data structures that make up the signed data in a Bitcoin transaction.
module Simplicity.Bitcoin
  ( parseLock, parseSequence
  ) where

import Data.Bits (testBit)
import Data.Word (Word32, Word16)

parseLock :: Word32 -> Either Word32 Word32
parseLock v | v < 500000 = Left v
            | otherwise = Right v

parseSequence :: Word32 -> Maybe (Either Word16 Word16)
parseSequence v | testBit v 31 = Nothing
                | testBit v 22 = Just (Right (fromIntegral v))
                | otherwise    = Just (Left (fromIntegral v))
