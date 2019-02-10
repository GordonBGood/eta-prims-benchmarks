{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, Strict,
             FlexibleContexts, MagicHash, UnboxedTuples, BangPatterns #-}

module Main where

import Java
import Data.Bits
import GHC.Int
import GHC.ST
import GHC.Base (Object(..), Int(..), Int64#(..))
import GHC.Exts (uncheckedIShiftRL#)
import GHC.Exts as OldPrims
import GHC.Arr as DArrs
import Data.Array.Base as OUArr
-- import JavaGHC.Data.Array.Base as UArrs
-- import Data.Array.ST (runSTUArray)
import Control.Monad.ST (runST)

import JavaGHC.Prims
import JavaGHC.Data.Array.Base as NUArr

-- import Debug.Trace

foreign import java unsafe "@static java.lang.System.currentTimeMillis"
  currentTimeMillis :: IO Int64

data JLongObjectArray = JLongObjectArray @java.lang.Long[]
  deriving Class

instance JArray JLong JLongObjectArray

primesbenchJObjArr :: Int -> IO Int
primesbenchJObjArr lps = java $ do
  !cmpsts <- arrayFromList $
                (map toJava $ take 2048 $ repeat (0 :: Int64) :: [JLong])
  let
    loop x =
      if x <= 0 then
        let
          cntem i !c =
            if i >= 131072 then return c else do
              v <- withObject cmpsts $ aget (i `shiftR` 6)
              if (fromJava v :: Int64) .&. (1 `shiftL` (i .&. 63)) /= 0
                then cntem (i + 1) c
              else cntem (i + 1) (c + 1)
        in cntem 0 1
      else
        let
          loopi i =
            if i > 254 then loop (x - 1) else do
              v <- withObject cmpsts $ aget (i `shiftR` 6)
              if ((fromJava v) :: Int64) .&. (1 `shiftL` (i .&. 63)) /= 0 then loopi (i + 1) else
                let
                  !p = i + i + 3
                  !s0 = 2 * i * (i + 3) + 3
                  loopc c =
                    if c >= 131072 then loopi (i + 1) else do
                      let !w = c `shiftR` 6
                      v <- withObject cmpsts $ aget w
                      withObject cmpsts $ aset w $ toJava ((fromJava v) .|. ((1 :: Int64) `shiftL` (c .&. 63)))
                      loopc (c + p)
                in loopc s0
        in loopi 0
  loop lps

primesbenchPrims :: Int -> Int64
primesbenchPrims lps = runST$ ST $ \s0# ->
  let
    !(# !s1#, !cmpsts# #) = OldPrims.newArray# 2048# (0 :: Int64) s0#
    loop x sl# =
      if x <= 0 then
        let
          !(# !sl'#, !arr# #) = OldPrims.unsafeFreezeArray# cmpsts# sl#
          go 2048 !c = c
          go i@(I# i#) !c =
            let !(# v #) = OldPrims.indexArray# arr# i# in
            let !cnt = fromIntegral $ popCount v :: Int64 in
            go (i + 1) (c - cnt)
        in (# sl'#, go 0 131073 #)
      else
        let
          loopi i@(I# i#) si# =
            if i > 254 then loop (x - 1) si# else
            case OldPrims.readArray# cmpsts# (i# `uncheckedIShiftRL#` 6#) si# of
              !(# !si'#, v #) ->
                if v .&. (1 `shiftL` (i .&. 63)) /= 0 then loopi (i + 1) si# else
                let
                  !p = i + i + 3
                  !s0 = (i `shiftL` 1) * (i + 3) + 3
                  loopc c@(I# c#) !sc# =
                    if c >= 131072 then loopi (i + 1) sc# else
                    let !w# = c# `uncheckedIShiftRL#` 6# in
                    case OldPrims.readArray# cmpsts# w# sc# of
                      !(# !sc'#, !v #) ->
                        let !msk = 1 `shiftL` (c .&. 63) in
                        case OldPrims.writeArray# cmpsts# w# (v .|. msk) sc'# of
                          !sc''# -> loopc (c + p) sc''#
                in loopc s0 si'#
            in loopi 0 sl#
  in loop lps s1#

primesbenchArray :: Int -> Int64
primesbenchArray lps = findcnt 0 131073 where
  !composites = runST $ do
    !cmpsts <- unsafeThawSTArray $ DArrs.listArray (0,2047) $ repeat (0 :: Int64)
    let
      loop x =
        if x <= 0 then do
          !fcmpsts <- unsafeFreezeSTArray cmpsts
          return fcmpsts else
        let
          loopi i =
            if i > 254 then loop (x - 1) else do
              v <- DArrs.unsafeReadSTArray cmpsts (i `shiftR` 6)
              if v .&. (1 `shiftL` (i .&. 63)) /= 0 then loopi (i + 1) else
                let
                  !p = i + i + 3
                  !s0 = (i `shiftL` 1) * (i + 3) + 3
                  loopc c =
                    if c >= 131072 then loopi (i + 1) else
                    let !w = c `shiftR` 6 in do
                    let !msk = 1 `shiftL` (c .&. 63)
                    !v <- DArrs.unsafeReadSTArray cmpsts w -- strictness to the max!!!
                    !dmy <- DArrs.unsafeWriteSTArray cmpsts w (v .|. msk)
                    dmy `seq` loopc (c + p)
                in loopc s0
        in loopi 0
    loop lps
  findcnt i !c =
    if i >= 2048 then c else
    let !v = DArrs.unsafeAt composites i in
    let !cnt = fromIntegral $ popCount v :: Int64 in
    findcnt (i + 1) (c - cnt)

primesbenchJInt64Arr :: Int -> IO Int
primesbenchJInt64Arr lps = java $ do
  !cmpsts <- arrayFromList $ take 2048 $ repeat (0 :: Int64)
  let
    loop x =
      if x <= 0 then
        let
          cntem i !c =
            if i >= 2048 then return c else do
              v <- withObject cmpsts $ aget i
              cntem (i + 1) (c - popCount v)
        in cntem 0 131073
      else
        let
          loopi i =
            if i > 254 then loop (x - 1) else do
              v <- withObject cmpsts $ aget (i `shiftR` 6)
              if v .&. (1 `shiftL` (i .&. 63)) /= 0 then loopi (i + 1) else
                let
                  !p = i + i + 3
                  !s0 = 2 * i * (i + 3) + 3
                  loopc c =
                    if c >= 131072 then loopi (i + 1) else do
                      let !w = c `shiftR` 6
                      v <- withObject cmpsts $ aget w
                      withObject cmpsts $ aset w $ v .|. (1 `shiftL` (c .&. 63))
                      loopc (c + p)
                in loopc s0
        in loopi 0
  loop lps

primesbenchByteArrPrims :: Int -> Int
primesbenchByteArrPrims lps = runST$ ST $ \s0# ->
  let
    !(# !s1#, !cmpsts# #) = OldPrims.newPinnedByteArray# 16384# s0#
    loop x sl# =
      if x <= 0 then
        let
          !(# !sl'#, !arr# #) = OldPrims.unsafeFreezeByteArray# cmpsts# sl#
          go 16384 !c = c
          go i@(I# i#) !c =
            let !v# = OldPrims.indexInt8Array# arr# i# in
            let !v = fromIntegral $ I# v# :: Int8 in
            go (i + 1) (c - popCount v)
        in (# sl'#, go 0 131073 #)
      else
        let
          loopi i@(I# i#) si# =
            if i > 254 then loop (x - 1) si# else
            case OldPrims.readInt8Array# cmpsts# (i# `uncheckedIShiftRL#` 3#) si# of
              !(# !si'#, !v# #) ->
                case v# `andI#` (1# `uncheckedIShiftL#` (i# `andI#` 7#)) of
                  0# ->
                    let
                      p = i + i + 3
                      s0 = (i `shiftL` 1) * (i + 3) + 3
                      loopc c@(I# c#) !sc# =
                        if c >= 131072 then loopi (i + 1) sc# else
                        let w# = c# `uncheckedIShiftRL#` 3# in
                        case OldPrims.readInt8Array# cmpsts# w# sc# of
                          !(# !sc'#, !v# #) ->
                            let msk# = 1# `uncheckedIShiftL#` (c# `andI#` 7#) in
                            let nv# = v# `orI#` msk# in
                            case OldPrims.writeInt8Array# cmpsts# w# nv# sc'# of
                              !sc''# -> loopc (c + p) sc''#
                    in loopc s0 si'#
                  _  -> loopi (i + 1) si#
        in loopi 0 sl#
    init i@(I# i#) !sn# =
      if i >= 16384 then loop lps sn# else
      case OldPrims.writeInt8Array# cmpsts# i# 0# sn# of
        !sn'# -> init (i + 1) sn'#
  in init 0 s1#

primesbenchUArray :: Int -> Int
primesbenchUArray lps = findcnt 0 131073 where
  !composites = runST $ do
    !cmpsts <- OUArr.newArray (0,16384) (0 :: Int8) :: ST s (OUArr.STUArray s Int Int8)
    let
      loop x =
        if x <= 0 then case OUArr.unsafeFreezeSTUArray cmpsts of !stcmps -> stcmps else
        let
          loopi i =
            if i > 254 then loop (x - 1) else do
              !v <- OUArr.unsafeRead cmpsts (i `shiftR` 3)
              if v .&. (1 `shiftL` (i .&. 7)) /= 0 then loopi (i + 1) else
                let
                  !p = i + i + 3
                  !s0 = (i `shiftL` 1) * (i + 3) + 3
                  loopc c =
                    if c >= 131072 then loopi (i + 1) else
                    let !w = c `shiftR` 3 in do
                    let !msk = 1 `shiftL` (c .&. 7)
                    !v <- OUArr.unsafeRead cmpsts w -- strictness to the max!!!
                    !dmy <- OUArr.unsafeWrite cmpsts w (v .|. msk)
                    dmy `seq` loopc (c + p)
                in loopc s0
        in loopi 0
    loop lps
  findcnt i !c =
    if i >= 16384 then c else
    let !v = fromIntegral $ OUArr.unsafeAt composites i :: Int8 in
    findcnt (i + 1) (c - popCount v)

data ByteBuffer = ByteBuffer @java.nio.ByteBuffer
  deriving Class

data ByteOrder = ByteOrder @java.nio.ByteOrder
  deriving Class

data Int64Buffer = Int64Buffer @java.nio.LongBuffer
  deriving Class

{-# INLINE newBB #-}
foreign import java unsafe "@static java.nio.ByteBuffer.allocateDirect"
  newBB :: Int -> ByteBuffer

{-# INLINE nativeOrder #-}
foreign import java unsafe "@static java.nio.ByteOrder.nativeOrder"
  nativeOrder :: ByteOrder

foreign import java unsafe "@static @field java.nio.ByteOrder.LITTLE_ENDIAN"
  littleEndian :: ByteOrder

{-# INLINE order #-}
foreign import java unsafe order :: ByteBuffer -> ByteOrder -> ByteBuffer

{-# INLINE makeInt64BB #-}
foreign import java unsafe "asLongBuffer"
  makeInt64BB :: ByteBuffer -> Int64Buffer

{-# INLINE writeInt64BB #-}
foreign import java unsafe "putLong"
  writeInt64BB :: ByteBuffer -> Int -> Int64 -> ByteBuffer

{-# INLINE writeInt64Buffer #-}
{-
writeInt64Buffer :: ByteBuffer -> Int -> Int64 -> ByteBuffer
writeInt64Buffer bb i val = writeInt64BB bb (i `shiftL` 3) val
-}
foreign import java unsafe "put"
  writeInt64Buffer :: Int64Buffer -> Int -> Int64 -> Int64Buffer

{-# INLINE newInt64Buffer #-}
newInt64Buffer :: Int -> Int64 -> Int64Buffer
newInt64Buffer size initval =
  let
    i64bb = makeInt64BB $ order (newBB $ size `shiftL` 3) nativeOrder
    loop i =
      if i >= size then i64bb else
      case writeInt64Buffer i64bb i initval of
        !_ -> loop (i + 1)
  in loop 0

{-# INLINE readInt64BB #-}
foreign import java unsafe "getLong"
  readInt64BB :: ByteBuffer -> Int -> Int64

{-# INLINE readInt64Buffer #-}
{-
readInt64Buffer :: ByteBuffer -> Int -> Int64
readInt64Buffer bb i = readInt64BB bb (i `shiftL` 3)
-}
foreign import java unsafe "get"
  readInt64Buffer :: Int64Buffer -> Int -> Int64

primesbenchByteBuffer :: Int -> Int
primesbenchByteBuffer lps =
  let
    !cmpsts = newInt64Buffer 2048 0
    loop x =
      if x <= 0 then
        let
          go 2048 !c = c
          go i !c =
            let !v = readInt64Buffer cmpsts i :: Int64 in
            go (i + 1) (c - popCount v)
        in go 0 131073
      else
        let
          loopi i =
            if i > 254 then loop (x - 1) else
            case readInt64Buffer cmpsts (i `shiftR` 6) of
              !v ->
                case v .&. (1 `shiftL` (i .&. 63)) of
                  0 ->
                    let
                      !p = i + i + 3
                      !s0 = (i `shiftL` 1) * (i + 3) + 3
                      loopc c =
                        if c >= 131072 then loopi (i + 1) else
                        let !w = c `shiftR` 6 in
                        case readInt64Buffer cmpsts w of
                          !v ->
                            let !msk = 1 `shiftL` (c .&. 63) in
                            case writeInt64Buffer cmpsts w (v .|. msk) of
                              !_ -> loopc (c + p)
                    in loopc s0
                  _ -> loopi (i + 1)
        in loopi 0
  in loop lps

primesbenchNUInt8Array :: Int -> Int
primesbenchNUInt8Array lps = findcnt 0 131073 where
  !composites = runST $ do
    !cmpsts <- NUArr.newArray (0,16383) (0 :: Int8) :: ST s (NUArr.STUArray s Int Int8)
    let
      loop x =
        if x <= 0 then case NUArr.unsafeFreezeSTUArray cmpsts of !stcmps -> stcmps else
        let
          loopi i =
            if i > 254 then loop (x - 1) else do
              !v <- NUArr.unsafeRead cmpsts (i `shiftR` 3)
              if v .&. (1 `shiftL` (i .&. 7)) /= 0 then loopi (i + 1) else
                let
                  !p = i + i + 3
                  !s0 = (i `shiftL` 1) * (i + 3) + 3
                  loopc c =
                    if c >= 131072 then loopi (i + 1) else
                    let !w = c `shiftR` 3 in do
                    let !msk = 1 `shiftL` (c .&. 7)
                    !v <- NUArr.unsafeRead cmpsts w -- strictness to the max!!!
                    !dmy <- NUArr.unsafeWrite cmpsts w (v .|. msk)
                    dmy `seq` loopc (c + p)
                in loopc s0
        in loopi 0
    loop lps
  findcnt i !c =
    if i >= 16384 then c else
    let !v = NUArr.unsafeAt composites i in
    findcnt (i + 1) (c - popCount v)

primesbenchNUInt64Array :: Int -> Int
primesbenchNUInt64Array lps = findcnt 0 131073 where
  !composites = runST $ do
    !cmpsts <- NUArr.newArray (0,2047) (0 :: Int64) :: ST s (NUArr.STUArray s Int Int64)
    let
      loop x =
        if x <= 0 then
          case NUArr.unsafeFreezeSTUArray cmpsts of !stcmps -> stcmps else
        let
          loopi i =
            if i > 254 then loop (x - 1) else do
              !v <- NUArr.unsafeRead cmpsts (i `shiftR` 6)
              if v .&. (1 `shiftL` (i .&. 63)) /= 0 then loopi (i + 1) else
                let
                  !p = i + i + 3
                  !s0 = (i `shiftL` 1) * (i + 3) + 3
                  loopc c =
                    if c >= 131072 then loopi (i + 1) else
                    let !w = c `shiftR` 6 in do
                    let !msk = 1 `shiftL` (c .&. 63)
                    !v <- NUArr.unsafeRead cmpsts w -- strictness to the max!!!
                    !dmy <- NUArr.unsafeWrite cmpsts w (v .|. msk)
                    dmy `seq` loopc (c + p)
                in loopc s0
        in loopi 0
    loop lps
  findcnt i !c =
    if i >= 2048 then c else
    let !v = NUArr.unsafeAt composites i in
    findcnt (i + 1) (c - popCount v)

primesbenchNUFastArray :: Int -> Int
primesbenchNUFastArray lps = findcnt 0 131073 where
  !composites = runST $ do
    !cmpsts <- NUArr.newArray (0,16383) 0 :: ST s (NUArr.STUArray s Int Int8)
    let
      loop x =
        if x <= 0 then case NUArr.unsafeFreezeSTUArray cmpsts of !stcmps -> stcmps else
        let
          loopi i =
            if i > 254 then loop (x - 1) else do
              v <- NUArr.unsafeRead cmpsts (i `shiftR` 3)
              if v .&. (1 `shiftL` (i .&. 7)) /= 0 then loopi (i + 1) else
                let
                  !p = i + i + 3
                  !s0 = (i `shiftL` 1) * (i + 3) + 3
                  !slmt = min 131072 (s0 + (p `shiftL` 3))
                  loops s =
                    if s >= slmt then loopi (i + 1) else
                    let
                      !msk = 1 `shiftL` (s .&. 7)
                      loopc c =
                        if c >= 16384 then loops (s + p) else do
                        !v <- NUArr.unsafeRead cmpsts c -- strictness to the max!!!
                        !dmy <- NUArr.unsafeWrite cmpsts c (v .|. msk)
                        dmy `seq` loopc (c + p)
                    in loopc (s `shiftR` 3)
                in loops s0
        in loopi 0
    loop lps
  findcnt i !c =
    if i >= 16384 then c else
    let !v = NUArr.unsafeAt composites i in
    findcnt (i + 1) (c - popCount v)

primesbenchNUBoolArray :: Int -> Int64
primesbenchNUBoolArray lps = findcnt 0 131073 where
  !composites = runST $ do
    !cmpsts <- NUArr.newArray (0,131071) False :: ST s (NUArr.STUArray s Int Bool)
    !cmpwrds <- (NUArr.castSTUArray :: NUArr.STUArray s Int Bool -> ST s (NUArr.STUArray s Int Word)) cmpsts
    let
      loop x =
        if x <= 0 then
          case NUArr.unsafeFreezeSTUArray cmpwrds of !stcmps -> stcmps else
        let
          loopi i =
            if i > 254 then loop (x - 1) else do
              !v <- NUArr.unsafeRead cmpsts i
              if v then loopi (i + 1) else
                let
                  !p = i + i + 3
                  !s0 = (i `shiftL` 1) * (i + 3) + 3
                  loopc c =
                    if c >= 131072 then loopi (i + 1) else do
                      !dmy <- NUArr.unsafeWrite cmpsts c True
                      dmy `seq` loopc (c + p)
                in loopc s0
        in loopi 0
    loop lps
  findcnt i !c =
    if i >= 4096 then c else
    let !cnt = popCount $ NUArr.unsafeAt composites i in
    findcnt (i + 1) (c - fromIntegral cnt)

main :: IO ()
main = do
  cnt <- primesbenchJObjArr 100 -- warm up to trigger JIT
  print cnt
  strt <- currentTimeMillis
  fcnt <- primesbenchJObjArr 100
  stop <- currentTimeMillis
  print fcnt
  putStrLn $ "Java Object Array:  " ++ show (stop - strt) ++ " milliseconds."
  print $ primesbenchPrims 100 -- warm up to trigger JIT
  strtop <- currentTimeMillis
  let cntop = primesbenchPrims 100
  stopop <- currentTimeMillis
  print cntop
  putStrLn $ "Current primitives:  " ++ show (stopop - strtop) ++ " milliseconds."
  print $ primesbenchArray 100 -- warm up to trigger JIT
  strtarr <- currentTimeMillis
  let cntarr = primesbenchArray 100
  stoparr <- currentTimeMillis
  print cntarr
  putStrLn $ "Current through Array:  " ++ show (stoparr - strtarr) ++ " milliseconds."
  putStrLn "The above is as fast using current Eta Boxed arrays as through FFI"
  print $ primesbenchByteArrPrims 100 -- warm up to trigger JIT
  strtbap <- currentTimeMillis
  let cntbap = primesbenchByteArrPrims 100
  stopbap <- currentTimeMillis
  print cntbap
  putStrLn $ "Current ByteArray primitives:  " ++ show (stopbap - strtbap) ++ " milliseconds."
  print $ primesbenchUArray 100 -- warm up to trigger JIT
  strtuarr <- currentTimeMillis
  let cntuarr = primesbenchUArray 100
  stopuarr <- currentTimeMillis
  print cntuarr
  putStrLn $ "Current through Int64 Unboxed Array:  " ++ show (stopuarr - strtuarr) ++ " milliseconds."
  putStrLn "The above using Eta Unboxed Array is slower than using Boxed Array and"
  putStrLn "much slower than using FFI - therefore unacceptible!!!\n"

  putStrLn "The above were 100 loops; the rest are 1000 loops:"
  jcnt <- primesbenchJInt64Arr 100 -- warm up to trigger JIT
  print jcnt
  strtjarr <- currentTimeMillis
  cntjarr <- primesbenchJInt64Arr 1000
  stopjarr <- currentTimeMillis
  print cntjarr
  putStrLn $ "Current through Int64 Java Array:  " ++ show (stopjarr - strtjarr) ++ " milliseconds."
  print $ primesbenchByteBuffer 100 -- warm up to trigger JIT
  strtbb <- currentTimeMillis
  let cntbb = primesbenchByteBuffer 1000
  stopbb <- currentTimeMillis
  print cntbb
  putStrLn $ "Simple proposal using Int64 Byte Buffer:  " ++ show (stopbb - strtbb) ++ " milliseconds."
  putStrLn "The above shows that using ByteBuffer is only about 40% slower than directly using Java Arrays!"
  putStrLn "This is the speed we are looking for..."
  print $ primesbenchNUInt8Array 100 -- warm up to trigger JIT
  strtnuarr <- currentTimeMillis
  let cntnuarr = primesbenchNUInt8Array 1000
  stopnuarr <- currentTimeMillis
  print cntnuarr
  putStrLn $ "Current through new Int8 Unboxed Array:  " ++ show (stopnuarr - strtnuarr) ++ " milliseconds."
  putStrLn "The above using Eta new Unboxed Array is almost as fast as FFI and very good"
  putStrLn "although this is for Int8 and therefore somewhat faster than Int64."
  putStrLn "It's also sometimes about twice as slow for what reasons; garbage collection???"
  print $ primesbenchNUInt64Array 100 -- warm up to trigger JIT
  strtnu64arr <- currentTimeMillis
  let cntnu64arr = primesbenchNUInt64Array 1000
  stopnu64arr <- currentTimeMillis
  print cntnu64arr
  putStrLn $ "Current through new Int64 Unboxed Array:  " ++ show (stopnu64arr - strtnu64arr) ++ " milliseconds."
  putStrLn "The above using Eta new Unboxed Array is almost as fast as FFI and very good."
  putStr "This is "
  putStr $ show (10 * fromIntegral (stopuarr - strtuarr) / fromIntegral (stopnu64arr - strtnu64arr))
  putStrLn " times better!"
  print $ primesbenchNUFastArray 100 -- warm up to trigger JIT
  strtnufarr <- currentTimeMillis
  let cntnufarr = primesbenchNUFastArray 1000
  stopnufarr <- currentTimeMillis
  print cntnufarr
  putStrLn $ "Through the new Unboxed Array fast algorithm:  " ++ show (stopnufarr - strtnufarr) ++ " milliseconds."
  putStrLn "The above using Eta new Unboxed Array is only about 50% slower than Kotlin or Scala."
  putStrLn "This shows there is little advantage to sophisticated algorithms when using ByteBuffer backed storage."
  print $ primesbenchNUBoolArray 100 -- warm up to trigger JIT
  strtnubarr <- currentTimeMillis
  let cntnubarr = primesbenchNUBoolArray 1000
  stopnubarr <- currentTimeMillis
  print cntnubarr
  putStrLn $ "Through the new Unboxed Bool Array native bit packing:  " ++ show (stopnubarr - strtnubarr) ++ " milliseconds."
  putStrLn "The above using Eta new Bool bit-packed Array is somewhat slower than doing it manually."
  putStrLn "This is due to nested function calls in the implementation of the bit packing."
