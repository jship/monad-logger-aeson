# Change log

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
