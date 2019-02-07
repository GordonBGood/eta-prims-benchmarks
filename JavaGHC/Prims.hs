-- replacement Prims related to direct ByteArray# and MutableByteArray# use...

{-# LANGUAGE BangPatterns, CPP, RankNTypes, MagicHash, UnboxedTuples, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, DeriveDataTypeable, UnliftedFFITypes #-}
{-# OPTIONS_HADDOCK hide #-}

module JavaGHC.Prims where

import GHC.Base (Class(..), Int(..), State#(..), Int#(..), Int64#(..))
import GHC.Exts ((+#), (<#), (==#), isTrue#, unsafeCoerce#, Addr#)
import GHC.Int (Int8(..), Int64(..))
import Data.Bits
import Java (Byte(..))

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
foreign import java unsafe "getAddress"
  byteArrayContents :: ByteArray# -> Int64

{-# INLINE byteArrayContents# #-}
byteArrayContents# :: ByteArray# -> Addr#
byteArrayContents# arr# =
  case fromIntegral $ byteArrayContents arr# of !(I64# a#) -> unsafeCoerce# a#

{-# INLINE readInt8Array #-}
foreign import java unsafe "get"
  readInt8Array :: MutableByteArray# s# -> Int -> Byte

{-# INLINE readInt8Array# #-}
readInt8Array#
    :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
readInt8Array# marr# i# s# =
  let !i = I# i# in
  case fromIntegral $ readInt8Array marr# i of !(I# v#) -> (# s#, v# #)

{-# INLINE writeInt8Array #-}
foreign import java unsafe "put"
  writeInt8Array :: MutableByteArray# s# -> Int -> Byte -> MutableByteArray# s#

{-# INLINE writeInt8Array# #-}
writeInt8Array#
    :: MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
writeInt8Array# marr# i# v# s# =
  let !i = I# i# in let !v = I# v# in
  case writeInt8Array marr# i (fromIntegral v) of !_ -> s#

{-# INLINE indexInt8Array #-}
foreign import java unsafe "get"
  indexInt8Array :: ByteArray# -> Int -> Byte

{-# INLINE indexInt8Array# #-}
indexInt8Array# :: ByteArray# -> Int# -> Int#
indexInt8Array# arr# i# =
  let !i = I# i# in
  case fromIntegral $ indexInt8Array arr# i of !(I# v#) -> v#

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
        0# -> si#
        _  ->
          case writeInt8Array# marr# i# val# si# of
            !si'# -> loop (i# +# 1#) si'#
  in loop ofst# s#

{-# INLINE sameMutableByteArray# #-}
sameMutableByteArray# :: MutableByteArray# d -> MutableByteArray# d -> Int#
sameMutableByteArray# marr1# marr2# =
  let
    sz1# = sizeofMutableByteArray# marr1#
    sz2# = sizeofMutableByteArray# marr2#
    arr1# = unsafeCoerce# marr1#
    arr2# = unsafeCoerce# marr2#
    loop i# =
      case i# <# sz1# of
        0# -> 1#
        _  ->
          case indexInt8Array# arr1# i# ==# indexInt8Array# arr2# i# of
            0# -> 0#
            _  -> loop (i# +# 1#)
  in if isTrue# (sz1# ==# sz2#) then loop 0# else 0#

{-# INLINE unsafeFreezeByteArray# #-}
unsafeFreezeByteArray# :: MutableByteArray# d -> State# d -> (# State# d, ByteArray# #)
unsafeFreezeByteArray# marr# s# = (# s#, unsafeCoerce# marr# #)