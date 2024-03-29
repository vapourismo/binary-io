# binary-io

## 0.6.2

* Revert changes from 0.6.0 in order to preserve write atomicity
* Implement StationaryReader which powers Reader without concurrency primitives

## 0.6.1

* Remove eta-reductions that cause GHC 9 builds to fail

## 0.6.0

* Respect chunking from the underlying PutM when writing resulting ByteStrings

## 0.5.0

* Let runPut and company return the result of the PutM operation

## 0.4.0

* Add module Data.Binary.IO.Lifted module which contains versions of all functions and types
  for the use in functors other than IO
* Make all functions and types in Data.Binary.IO aliases of the ones in the Lifted module
* Reduce allocations for Await-Notify pairs

## 0.3.0

* Add isEmpty function to check if a read source has no more input available
* Enhance newPipe to produce a Reader that detects when its paired Writer went out-of-scope

## 0.2.0

* Add newPipe function that can create a connected Reader and Writer
* Make Reader more resilient against exceptions that may happen in the provided Get operation

## 0.1.1

* Add functions to create a Reader, Writer or Duplex without a Handle

## 0.1.0

* Remove continuation parameter to 'runGet' and 'readWith'

## 0.0.1

* Inception
