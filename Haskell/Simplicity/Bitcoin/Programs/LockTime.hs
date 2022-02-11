module Simplicity.Bitcoin.Programs.LockTime
 ( txIsFinal
 , txLockHeight, txLockTime
 , txLockDistance, txLockDuration
 , checkLockHeight, checkLockTime
 , checkLockDistance, checkLockDuration
 , module Simplicity.Programs.LockTime
 , Bit
 ) where

import Prelude hiding (Word, all, drop, max, not, take)

import Simplicity.Bitcoin.Primitive
import Simplicity.Bitcoin.Term
import Simplicity.Programs.Arith
import Simplicity.Programs.Bit
import Simplicity.Programs.Generic
import Simplicity.Programs.LockTime
import Simplicity.Programs.Word

txIsFinal :: (Core term, Primitive term) => term () Bit
txIsFinal = (unit &&& unit) >>> forWhile word32 body >>> copair iden true
 where
  body = take (drop (primitive InputSequence)) >>> copair (injl true) (all word32 >>> copair (injl false) (injr unit))

txLockHeight :: (Core term, Primitive term) => term () Height
txLockHeight = txIsFinal &&& primitive LockTime
           >>> cond z (parseLock >>> (copair iden z))
 where
  z = unit >>> zero word32

txLockTime :: (Core term, Primitive term) => term () Time
txLockTime = txIsFinal &&& primitive LockTime
         >>> cond z (parseLock >>> (copair z iden))
 where
  z = unit >>> zero word32

bip68VersionCheck :: (Core term, Primitive term) => term () Bit
bip68VersionCheck = scribe (toWord32 2) &&& primitive Version >>> le word32

txLockDistance :: (Core term, Primitive term) => term () Distance
txLockDistance = bip68VersionCheck &&& zero word16
             >>> match ih (forWhile word32 body >>> copair iden iden)
 where
  body = take (drop (primitive InputSequence)) &&& ih
     >>> match (injl ih) (injr (take parseSequence &&& ih >>> match ih (match (max word16) ih)))

txLockDuration :: (Core term, Primitive term) => term () Duration
txLockDuration = bip68VersionCheck &&& zero word16
             >>> match ih (forWhile word32 body >>> copair iden iden)
 where
  body = take (drop (primitive InputSequence)) &&& ih
     >>> match (injl ih) (injr (take parseSequence &&& ih >>> match ih (match ih (max word16))))

checkLockHeight :: (Assert term, Primitive term) => term Height ()
checkLockHeight = assert (iden &&& (unit >>> txLockHeight) >>> le word32)

checkLockTime :: (Assert term, Primitive term) => term Time ()
checkLockTime = assert (iden &&& (unit >>> txLockTime) >>> le word32)

checkLockDistance :: (Assert term, Primitive term) => term Distance ()
checkLockDistance = assert (iden &&& (unit >>> txLockDistance) >>> le word16)

checkLockDuration :: (Assert term, Primitive term) => term Duration ()
checkLockDuration = assert (iden &&& (unit >>> txLockDuration) >>> le word16)
