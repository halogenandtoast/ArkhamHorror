-- |
--
-- A drop-in replacement for @"TestImport"@ but exporting lifted versions of all
--
module TestImport.Lifted
  ( module X
  ) where

import TestImport as X hiding
  ( expectationFailure
  , shouldBe
  , shouldContain
  , shouldEndWith
  , shouldMatchList
  , shouldNotBe
  , shouldNotContain
  , shouldNotReturn
  , shouldNotSatisfy
  , shouldReturn
  , shouldSatisfy
  , shouldStartWith
  )

import Test.Hspec.Expectations.Lifted as X
  ( expectationFailure
  , shouldBe
  , shouldContain
  , shouldEndWith
  , shouldMatchList
  , shouldNotBe
  , shouldNotContain
  , shouldNotReturn
  , shouldNotSatisfy
  , shouldReturn
  , shouldSatisfy
  , shouldStartWith
  )
