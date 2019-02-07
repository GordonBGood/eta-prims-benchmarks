# Proof of Concept for speeding up Eta Unboxed primitive array access...

- [Proof of Concept for speeding up Eta Unboxed primitive array access...](#proof-of-concept-for-speeding-up-eta-unboxed-primitive-array-access)
  - [Introduction](#introduction)
  - [Proposed method to solve the problem...](#proposed-method-to-solve-the-problem)
  - [Testing methodology to ascertain results...](#testing-methodology-to-ascertain-results)
  - [Results](#results)
  - [Extentions to this work...](#extentions-to-this-work)
  - [Conclusion](#conclusion)
  - [Appendix - Expected Output (on a 1.92 Gigahertz CPU)](#appendix---expected-output-on-a-192-gigahertz-cpu)

## Introduction

It was observed that using the `array` library with `UArray`/`STUArray` for immutable/mutable arrays is currently slower even than using Boxed arrays containing primitives as in `Array`/`STArray` (no 'U').

The reason for this is that Eta currently allocates memory for arrays using an emulation of pointers using the Java `ByteBuffer` class as a backing store, which is fine when necessary, but it then makes all accesses to these allocated unboxed arrays through this pointer emulation which is about ten times slower than not using pointers and unnecessary when directly accessing the arrays through indices.

The code in this repository seeks to show a proof of concept of how to rectify this slow access.

## Proposed method to solve the problem...

At first it was thought that the method should involve changing the patches for the `array` package in `eta-hackage` but that is complex and would mean that every packages that uses unboxed arrays would have to be patched in a similar complex manner.

The method proposed in this PoC is to change all primitive ops that access unboxed arrays NOT using pointer access (the most common case) by directly accessing the Java `ByteBuffer`'s produced by the emulated allocation.  Although this is slower (by about 40% or a little more for larger multi-byte primitives) than using Java primitive arrays as the backing store, it produces maximum compatibilty with the Haskell/Eta primitive ops standard as to these buffers still having an emulated "address" pointer for comparison and production to emulate access using these pointers for FFI/JNI purposes and for determination that the backing store is actually the identical shared data or not.

Minimum changes need be made to the `ByteArray` static methods to accommodate this change as to just the addition of additional method(s) to make the backing `ByteBuffer`'s exposed to the primitive ops and anything that is easier (and more direct) in Java than in Eta, such as storing and retrieving the emulated pointer in the eight bytes just above the end of the actual buffer data.  Most other required information is available from available fields and methods of the backing `ByteBuffer`'s.

The majority of the work is in re-writing all of the primitive ops that deal with `ByteArray#`/`MutableByteArray#` direction not through pointers, of which enough samples are provided in order to prove that the method works with the provided benchmark.

## Testing methodology to ascertain results...

The benchmark used is a simple bit-packed Sieve of Eratosthenes primes counting algorithm; this is appropriate to testing this as the pattern of primes is pseudo random and therefore can't be optimized away by the compiler, and the bit packing will serve to compare performance with the Eta primitive `Bool` unboxed array's, which are bit-packed by specification and will be implemented as such.

As well, Boxed arrays general speed is checked, which perform as expected and in fact faster than the FFI Java Object arrays due to the ST monad being more efficient than the Java monad, although both are slower than using Java arrays directly (which also breaks referential transparency).

## Results

About 7.5 times better performance than currently for large multi-byte primitives, and up to about 9.5 times faster using only single bytes.  This is still perhaps 40% slower than Java/Kotlin/Scala, but compatibility with Haskell packages cannot be sacrificed to get more so this is a worthwhile trade-off.  We may also get some slight extra gains of perhaps up to 20% when the primitives are implemented as part of the code generation due to not having go go through FFI Java class Wrappers Type's (JWT's).

The JWT's may also be causing sporadic Garbage Collection due to continuous allocation/deallocation which is sometimes making the benchmarks up to over twice as slow.

## Extentions to this work...

Further work once the primitive ops are implemented and tested is to look at the implementation of the Java arrays to eliminate the use of the Java monad and to make them more compatible with the Haskell/Eta infrastructure.  This will make them easier to use (for a programmer than knows Haskell/ETa), will make their use slightly faster, and will eliminate a problem currently where certain combinations of forcing strictness causes them to be non referentially transparent due to how the Java monad is implemented.

When this is completed, programmers writing new Eta code that doesn't need to rely on `eta-hackage` packages will be free to get array access speeds about the same speed as competing JVM languages such as Java/Scala/Kotlin/Clojure.

## Conclusion

Hopefully performance such as this will be enough to convince users of those other languages to convert to Eta for its absolutely "pure" functional paradygm, but antipate that it will be a bit of a hard "sell" for the remaining following reasons:
1. Clojure users may be the easiest to convert other than if they love Dynamic Types rather than Static ones.
2. Scala users may be the next easiest to convert as they are already used to trying for "pure" functional code in many cases.
3. Kotlin users will be hard to convince if they have never learned Haskell, as they have generally just been converted from Java to Kotlin because it has a simpler more direct syntax and supports some functional expressions of code (mostly lambdas and higher-order functions) and may be convinced that is all that's necessary; the small simple run time libraries may also be an argument.
4. Die-hard Java fans will be the hardest to convert as they have already resisted Kotlin and are convinced that Java itself is moving toward being a functional language (which is no more and perhaps less than Kotlin).
5. All of them are going to find it frustrating to fight the strictness analyser when laziness is not required or desired, which even turning on the `Strict` extension doesn't make the problems go away (or perhaps it has a bug?); perhaps some thought should go into whether Eta should be entirely compiled as a `Strict` language (including all libraries) with optional laziness as in for Lazy Lists or perhaps two versions, one `Strict` and optionally `Lazy` and one `Lazy` but optionally `Strict` as currently.  Many users will turn away after experiencing the problems of laziness.  Of course, increased really good documentation explaining laziness, why it is useful, and how to work around it when it isn't wanted, would certainly help, and I will likely get involved with that.

## Appendix - Expected Output (on a 1.92 Gigahertz CPU)

```
23000
23000
Java Object Array:  985 milliseconds.
23000
23000
Current primitives:  763 milliseconds.
23000
23000
Current through Array:  892 milliseconds.
The above is as fast using current Eta Boxed arrays as through FFI
23000
23000
Current ByteArray primitives:  1255 milliseconds.
23000
23000
Current through Int64 Unboxed Array:  1384 milliseconds.
The above using Eta Unboxed Array is slower than using Boxed Array and
much slower than using FFI - therefore unacceptible!!!

The above were 100 loops; the rest are 1000 loops:
23000
23000
Current through Int64 Java Array:  1056 milliseconds.
23000
23000
Simple proposal using Int64 Byte Buffer:  1378 milliseconds.
The above shows that using ByteBuffer is only about 40% slower than directly using Java Arrays!
This is the speed we are looking for...
true
23000
true
23000
Current through new Int8 Unboxed Array:  1475 milliseconds.
The above using Eta new Unboxed Array is almost as fast as FFI and very good
although this is for Int8 and therefore somewhat faster than Int64.
It's also sometimes about twice as slow for what reasons; garbage collection???
true
23000
true
23000
Current through new Int64 Unboxed Array:  1795 milliseconds.
The above using Eta new Unboxed Array is almost as fast as FFI and very good.
This is 7.710306406685237 times better!
true
23000
true
23000
Current through new Unboxed Array fast algorithm:  1570 milliseconds.
The above using Eta new Unboxed Array is only about 50% slower than Kotlin or Scala.```