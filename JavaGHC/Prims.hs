-- replacement Prims related to direct ByteArray# and MutableByteArray# use...

{-# LANGUAGE Strict, BangPatterns, CPP, RankNTypes, MagicHash, UnboxedTuples,
             MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,
             DeriveDataTypeable, UnliftedFFITypes #-}
{-# OPTIONS_HADDOCK hide #-}

module JavaGHC.Prims where

import GHC.Base (Class(..))
import GHC.Exts (State#(..), Int#(..), Int32#(..), Int64#(..),
                 Word#(..), Word32#(..), Word64#(..), nullAddr#,
                 Addr#(..), StablePtr#(..), Ptr(..), FunPtr(..),
                 Int(..), Word(..), Float(..), Double(..), Char(..),
                 (+#), (<#), (==#), isTrue#, unsafeCoerce#,
                 shiftL#, shiftRL#, iShiftL#, iShiftRA#, iShiftRL#)
import GHC.Stable
import GHC.Int (Int8(..), Int16(..), Int32(..), Int64(..))
import GHC.Word (Word8(..), Word16(..), Word32(..), Word64(..))
import Java (Byte(..), Short(..))
import Data.Bits

data ByteArray# = ByteArray# @java.nio.ByteBuffer
  deriving Class

newtype MutableByteArray# s# = MutableByteArray# ByteArray#

{-# INLINE newCreate #-}
foreign import java unsafe "@static eta.runtime.io.ByteArray.createUnpinned"
  newCreate :: Int -> MutableByteArray# s#

{-# INLINE newByteArray# #-}
newByteArray# :: Int# -> State# d -> (# State# d, MutableByteArray# d #)
newByteArray# n# s# =
  let !n = I# n# in case newCreate n of !marr# -> (# s#, marr# #)

{-# INLINE isByteArrayPinned #-}
foreign import java unsafe "direct"
  isByteArrayPinned :: ByteArray# -> Bool

{-# INLINE isByteArrayPinned# #-}
isByteArrayPinned# :: ByteArray# -> Int#
isByteArrayPinned# arr# =
  case isByteArrayPinned arr# of !b -> if b then 1# else 0#

{-# INLINE isMutableByteArrayPinned #-}
foreign import java unsafe "@static eta.runtime.io.ByteArray.isPinned"
  isMutableByteArrayPinned :: MutableByteArray# s# -> Int

{-# INLINE isMutableByteArrayPinned# #-}
isMutableByteArrayPinned# :: MutableByteArray# d -> Int#
isMutableByteArrayPinned# marr# =
  case isMutableByteArrayPinned marr# of !(I# b#) -> b#

{-# INLINE sizeofByteArray #-}
foreign import java unsafe "limit"
  sizeofByteArray :: ByteArray# -> Int

{-# INLINE sizeofByteArray# #-}
sizeofByteArray# :: ByteArray# -> Int#
sizeofByteArray# arr# =
  case sizeofByteArray arr# of !(I# sz#) -> sz#

{-# INLINE sizeofMutableByteArray #-}
foreign import java unsafe "limit"
  sizeofMutableByteArray :: MutableByteArray# s# -> Int

{-# INLINE sizeofMutableByteArray# #-}
sizeofMutableByteArray# :: MutableByteArray# d -> Int#
sizeofMutableByteArray# marr# =
  case sizeofMutableByteArray marr# of !(I# sz#) -> sz#

{-# INLINE getSizeofMutableByteArray# #-}
getSizeofMutableByteArray#
    :: MutableByteArray# d -> State# d -> (# State# d, Int# #)
getSizeofMutableByteArray# marr# s# =
  case sizeofMutableByteArray marr# of !(I# sz#) -> (# s#, sz# #)

{-# INLINE byteArrayContents #-}
foreign import java unsafe "@static eta.runtime.io.ByteArray.getAddress"
  byteArrayContents :: ByteArray# -> Int64

{-# INLINE byteArrayContents# #-}
byteArrayContents# :: ByteArray# -> Addr#
byteArrayContents# arr# =
  case fromIntegral $ byteArrayContents arr# of !(I64# a#) -> unsafeCoerce# a#

{-# INLINE mutableByteArrayContents #-}
foreign import java unsafe "@static eta.runtime.io.ByteArray.getAddress"
  mutableByteArrayContents :: MutableByteArray# s# -> Int64

{-# INLINE sameMutableByteArray# #-}
sameMutableByteArray# :: MutableByteArray# d -> MutableByteArray# d -> Int#
sameMutableByteArray# marr1# marr2# =
  case fromIntegral $ mutableByteArrayContents marr1# of
     !addr1 ->
        case fromIntegral $ mutableByteArrayContents marr2# of
          !addr2 ->
              if addr1 == addr2 then 1# else 0#

{-# INLINE readInt8Array #-}
foreign import java unsafe "get"
  readInt8Array :: MutableByteArray# s# -> Int -> Byte

{-# INLINE readInt8Array# #-}
readInt8Array#
    :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
readInt8Array# marr# i# s# =
  let !i = I# i# in
  case fromIntegral $ readInt8Array marr# i of !(I# v#) -> (# s#, v# #)

{-# INLINE readWord8Array# #-}
readWord8Array#
    :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
readWord8Array# marr# i# s# =
  let !i = I# i# in
  case fromIntegral $ readInt8Array marr# i of !(W# v#) -> (# s#, v# #)

{-# INLINE writeInt8Array #-}
foreign import java unsafe "put"
  writeInt8Array :: MutableByteArray# s# -> Int -> Byte -> MutableByteArray# s#

{-# INLINE writeInt8Array# #-}
writeInt8Array#
    :: MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
writeInt8Array# marr# i# v# s# =
  let !i = I# i# in let !v = I# v# in
  case writeInt8Array marr# i (fromIntegral v) of !_ -> s#

{-# INLINE writeWord8Array# #-}
writeWord8Array#
    :: MutableByteArray# d -> Int# -> Word# -> State# d -> State# d
writeWord8Array# marr# i# v# s# =
  let !i = I# i# in let !v = fromIntegral (W# v#) in
  case writeInt8Array marr# i v of !_ -> s#

{-# INLINE indexInt8Array #-}
foreign import java unsafe "get"
  indexInt8Array :: ByteArray# -> Int -> Byte

{-# INLINE indexInt8Array# #-}
indexInt8Array# :: ByteArray# -> Int# -> Int#
indexInt8Array# arr# i# =
  let !i = I# i# in
  case fromIntegral $ indexInt8Array arr# i of !(I# v#) -> v#

{-# INLINE indexWord8Array# #-}
indexWord8Array# :: ByteArray# -> Int# -> Word#
indexWord8Array# arr# i# =
  let !i = I# i# in
  case fromIntegral $ indexInt8Array arr# i of !(W# v#) -> v#

{-# INLINE readIntArray #-}
foreign import java unsafe "getInt"
  readIntArray :: MutableByteArray# s# -> Int -> Int

{-# INLINE readIntArray# #-}
readIntArray#
    :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
readIntArray# marr# i# s# =
  let !i = (I# i#) `shiftL` 2 in
  case readIntArray marr# i of !(I# v#) -> (# s#, v# #)

{-# INLINE readInt32Array# #-}
readInt32Array#
    :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
readInt32Array# = readIntArray#

{-# INLINE readWordArray# #-}
readWordArray#
    :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
readWordArray# marr# i# s# =
  let !i = (I# i#) `shiftL` 2 in
  case fromIntegral $ readIntArray marr# i of !(W# v#) -> (# s#, v# #)

{-# INLINE readWord32Array# #-}
readWord32Array#
    :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
readWord32Array# = readWordArray#

{-# INLINE writeIntArray #-}
foreign import java unsafe "putInt"
  writeIntArray :: MutableByteArray# s# -> Int -> Int -> MutableByteArray# s#

{-# INLINE writeIntArray# #-}
writeIntArray#
    :: MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
writeIntArray# marr# i# v# s# =
  let !i = (I# i#) `shiftL` 2 in let !v = I# v# in
  case writeIntArray marr# i v of !_ -> s#

{-# INLINE writeInt32Array# #-}
writeInt32Array#
    :: MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
writeInt32Array# = writeIntArray#

{-# INLINE writeWordArray# #-}
writeWordArray#
    :: MutableByteArray# d -> Int# -> Word# -> State# d -> State# d
writeWordArray# marr# i# v# s# =
  let !i = (I# i#) `shiftL` 2 in let !v = fromIntegral (W# v#) in
  case writeIntArray marr# i v of !_ -> s#

{-# INLINE writeWord32Array# #-}
writeWord32Array#
    :: MutableByteArray# d -> Int# -> Word# -> State# d -> State# d
writeWord32Array# = writeWordArray#

{-# INLINE indexIntArray #-}
foreign import java unsafe "getInt"
  indexIntArray :: ByteArray# -> Int -> Int

{-# INLINE indexIntArray# #-}
indexIntArray# :: ByteArray# -> Int# -> Int#
indexIntArray# arr# i# =
  let !i = (I# i#) `shiftL` 2 in
  case indexIntArray arr# i of !(I# v#) -> v#

{-# INLINE indexInt32Array# #-}
indexInt32Array# :: ByteArray# -> Int# -> Int#
indexInt32Array# = indexIntArray#

{-# INLINE indexWordArray# #-}
indexWordArray# :: ByteArray# -> Int# -> Word#
indexWordArray# arr# i# =
  let !i = (I# i#) `shiftL` 2 in
  case fromIntegral $ indexIntArray arr# i of !(W# v#) -> v#
--
{-# INLINE indexWord32Array# #-}
indexWord32Array# :: ByteArray# -> Int# -> Word#
indexWord32Array# = indexWordArray#

{-# INLINE readInt64Array #-}
foreign import java unsafe "getLong"
  readInt64Array :: MutableByteArray# s# -> Int -> Int64

{-# INLINE readInt64Array# #-}
readInt64Array#
    :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Int64# #)
readInt64Array# marr# i# s# =
  let !i = (I# i#) `shiftL` 3 in
  case fromIntegral $ readInt64Array marr# i of
    !(I64# v#) -> (# s#, v# #)

{-# INLINE writeInt64Array #-}
foreign import java unsafe "putLong"
  writeInt64Array
      :: MutableByteArray# s# -> Int -> Int64 -> MutableByteArray# s#

{-# INLINE writeInt64Array# #-}
writeInt64Array#
    :: MutableByteArray# d -> Int# -> Int64# -> State# d -> State# d
writeInt64Array# marr# i# v# s# =
  let !i = (I# i#) `shiftL` 3 in let !v = I64# v# in
  case writeInt64Array marr# i v of !_ -> s#

{-# INLINE indexInt64Array #-}
foreign import java unsafe "getLong"
  indexInt64Array :: ByteArray# -> Int -> Int64

{-# INLINE indexInt64Array# #-}
indexInt64Array# :: ByteArray# -> Int# -> Int64#
indexInt64Array# arr# i# =
  let !i = (I# i#) `shiftL` 3 in
  case fromIntegral $ indexInt64Array arr# i of !(I64# v#) -> v#

{-# INLINE setByteArray# #-}
setByteArray#
    :: MutableByteArray# d -> Int# -> Int# -> Int# -> State# d -> State# d
setByteArray# marr# ofst# len# val# s# =
  let
    lmt# = ofst# +# len#
    loop i# !si# =
      case i# <# lmt# of
        !0# -> si#
        !_  ->
          case writeInt8Array# marr# i# val# si# of
            !si'# -> loop (i# +# 1#) si'#
  in loop ofst# s#

{-# INLINE unsafeFreezeByteArray# #-}
unsafeFreezeByteArray# :: MutableByteArray# d -> State# d -> (# State# d, ByteArray# #)
unsafeFreezeByteArray# marr# s# = (# s#, unsafeCoerce# marr# #)