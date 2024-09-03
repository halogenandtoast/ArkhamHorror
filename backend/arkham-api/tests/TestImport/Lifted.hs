{- |

A drop-in replacement for @"TestImport"@ but exporting lifted versions of all
-}
module TestImport.Lifted (
  module X,
  module TestImport.Lifted,
) where

import TestImport as X hiding (
  assertNone,
  expectationFailure,
  pending,
  shouldBe,
  shouldContain,
  shouldEndWith,
  shouldMatchList,
  shouldNotBe,
  shouldNotContain,
  shouldNotReturn,
  shouldNotSatisfy,
  shouldReturn,
  shouldSatisfy,
  shouldStartWith,
 )

import Test.Hspec.Expectations.Lifted as X (
  expectationFailure,
  shouldBe,
  shouldContain,
  shouldEndWith,
  shouldMatchList,
  shouldNotBe,
  shouldNotContain,
  shouldNotReturn,
  shouldNotSatisfy,
  shouldReturn,
  shouldSatisfy,
  shouldStartWith,
 )

import TestImport qualified as TI

pending :: MonadIO m => m ()
pending = liftIO TI.pending
