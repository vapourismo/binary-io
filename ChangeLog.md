# binary-io

## 0.2.1

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
