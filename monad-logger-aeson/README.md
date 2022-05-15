# [monad-logger-aeson][]

[![Build badge][]][build]
[![Version badge][]][version]

## Synopsis

`monad-logger-aeson` provides structured JSON logging using `monad-logger`'s
interface. Specifically, it is intended to be a drop-in replacement for
`monad-logger`'s `Control.Monad.Logger.CallStack` module.

For additional detail on the library, please see the [Haddocks][], the
[announcement blog post][], and the remainder of this README.

## Crash course

Assuming we have the following `monad-logger`-based application and library code
(shown in the same snippet here for brevity's sake):

```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import Control.Monad.Logger.CallStack
import Data.Text (pack)

doStuff :: (MonadLogger m) => Int -> m ()
doStuff x = do
  logDebug $ "Doing stuff: x=" <> pack (show x)

main :: IO ()
main = do
  runStdoutLoggingT do
    doStuff 42
    logInfo "Done"
```

We would get something like this log output:

```text
[Debug] Doing stuff: x=42 @(main:Main app/readme-example.hs:12:3)
[Info] Done @(main:Main app/readme-example.hs:18:5)
```

We can change our import from this:

```haskell
import Control.Monad.Logger.CallStack
```

To this:

```haskell
import Control.Monad.Logger.Aeson
```

We'll have one compiler error to address:

```text
monad-logger-aeson/app/readme-example.hs:12:35: error:
    • Couldn't match expected type ‘Message’
                  with actual type ‘Data.Text.Internal.Text’
    • In the second argument of ‘(<>)’, namely ‘pack (show x)’
      In the second argument of ‘($)’, namely
        ‘"Doing stuff: x=" <> pack (show x)’
      In a stmt of a 'do' block:
        logDebug $ "Doing stuff: x=" <> pack (show x)
   |
12 |   logDebug $ "Doing stuff: x=" <> pack (show x)
   |
```

Indicating that we need to provide the `logDebug` call a `Message` rather than a
`Text` value. This compiler error gives us a choice depending upon our current
time constraints: we can either go ahead and convert this `Text` value to a
"proper" `Message` by moving the metadata it encodes into structured data (i.e.
a `[Pair]` value), or we can defer doing that for now by tacking on an empty
`[Pair]` value. We'll opt for the former here:

```haskell
logDebug $ "Doing stuff" :# ["x" .@ x]
```

Note that the `logInfo` call did not give us a compiler error, as `Message` has
an `IsString` instance.

Our log output now looks like this (formatted for readability here with `jq`):

```jsonl
{
  "time": "2022-05-15T20:52:15.5559417Z",
  "level": "debug",
  "location": {
    "package": "main",
    "module": "Main",
    "file": "app/readme-example.hs",
    "line": 11,
    "char": 3
  },
  "message": {
    "text": "Doing stuff",
    "meta": {
      "x": 42
    }
  }
}
{
  "time": "2022-05-15T20:52:15.5560448Z",
  "level": "info",
  "location": {
    "package": "main",
    "module": "Main",
    "file": "app/readme-example.hs",
    "line": 17,
    "char": 5
  },
  "message": {
    "text": "Done"
  }
}
```

Voilà! Now our Haskell code is using structured logging. Our logs are now fit
for parsing, ingestion into our log aggregation/anaysis service of choice, etc.

## Goals

The following goals have underpinned the development of `monad-logger-aeson`:

1. Structured logging _must_ be easy to add to existing Haskell codebases
1. Structured logging _should_ be performant

We believe we have achieved goal 1 by targeting `monad-logger`'s
`MonadLogger`/`LoggingT` interface. There are many interesting logging libraries
to choose from in Haskell: `monad-logger`, `di`, `logging-effect`, `katip`, and
so on. Both by comparing the [reverse dependency list][] for `monad-logger` with
the other logging libraries' reverse dependency lists, and also consulting our
personal experiences working on Haskell codebases, `monad-logger` would seem to
be the most prevalent logging library in the wild. In developing our library as
a drop-in replacement for `monad-logger`, we hope to empower Haskellers using
this popular logging interface to be able to add structured logging to their
programs with minimal fuss.

We believe we have achieved goal 2 by directly representing in-flight log
messages using an `aeson` object `Encoding`, and by _never_ parsing these
in-flight log messages when assembling the final logged message. A more
straightforward implementation would attempt parsing of in-flight log messages
to know whether they came from `monad-logger-aeson` or `monad-logger`. Rather
than parse in-flight log messages, we leverage a simple-but-subtle tagging
scheme on in-flight messages such that we can O(1) know whether or not an
in-flight log message is a `monad-logger-aeson`-encoded message or a
`monad-logger`-encoded message. When assembling the final logged message, we
consult the tag to know how to incorprate the in-flight message's encoding into
the final logged message's encoding. While we believe the principles described
previously should provide good performance, please note that benchmarks do not
yet exist for this library. Caveat emptor!

[monad-logger-aeson]: https://github.com/jship/monad-logger-aeson
[Build badge]: https://github.com/jship/monad-logger-aeson/workflows/CI/badge.svg
[build]: https://github.com/jship/monad-logger-aeson/actions
[Version badge]: https://img.shields.io/hackage/v/monad-logger-aeson?color=brightgreen&label=version&logo=haskell
[version]: https://hackage.haskell.org/package/monad-logger-aeson
[Haddocks]: https://hackage.haskell.org/package/monad-logger-aeson
[announcement blog post]: https://jship.github.io
[reverse dependency list]: https://packdeps.haskellers.com/reverse/monad-logger