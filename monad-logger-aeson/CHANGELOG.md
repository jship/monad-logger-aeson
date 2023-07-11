# Change log

## 0.4.1.1

* Relax upper bound on `bytestring`: 0.12.0.0 -> 0.13.0.0

## 0.4.1.0

* Support `aeson-2.2`

## 0.4.0.4

* Relax upper bound on `fast-logger`: 2.11 -> 2.12

## 0.4.0.3

* Relax upper bound on `fast-logger`: 3.2.0 -> 3.3.0

## 0.4.0.2

* Relax upper bound on `time`: 1.13 -> 1.14

## 0.4.0.1

* Relax upper bound on `hspec`: 2.10 -> 2.11
* Relax upper bound on `time`: 1.12 -> 1.13

## 0.4.0.0

* Replace `log*N` variants reexported from `monad-logger` with custom variants
  that accept `Message` instead of `Text`

## 0.3.1.0

* Add `Semigroup` instance for `SeriesElem`

## 0.3.0.2

* Backwards compatibility down to GHC 8.4 (@pbrisbin)

## 0.3.0.1

* Minor Haddock updates

## 0.3.0.0

* Remove deprecated `(.@)` operator
* Message metadata is now of type `[SeriesElem]` instead of `[Series]`
  * _Should_ be backwards-compatible based on library's recommended usage

## 0.2.0.2

* Support `text-2.0`

## 0.2.0.1

* Relax dependency lower bounds to support GHC 8.8 (@pbrisbin)

## 0.2.0.0

* Deprecate `(.@)` in favor of `(.=)` from `aeson`
* Re-export `(.=)` from `aeson`

## 0.1.0.1

* Relax lower bound on `base`: 4.14.3.0 -> 4.14.0.0

## 0.1.0.0

* Initial release

## 0.0.0.0

* Unreleased dev version
