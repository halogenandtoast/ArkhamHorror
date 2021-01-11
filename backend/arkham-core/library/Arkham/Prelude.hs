module Arkham.Prelude
  ( module X
  , module Arkham.Prelude
  ) where

import ClassyPrelude as X hiding (on, (\\))

import Control.Lens as X
  ( Lens'
  , Traversal'
  , at
  , ix
  , lens
  , preview
  , to
  , traverseOf
  , traverseOf_
  , view
  , (%~)
  , (&)
  , (+~)
  , (-~)
  , (.~)
  , (?~)
  , (^.)
  , (^..)
  , (^?)
  )
import Control.Lens.TH as X
import Control.Monad.Extra as X (concatMapM)
import Control.Monad.Random as X (MonadRandom)
import Control.Monad.Random.Class as X (getRandom, getRandomR)
import Control.Monad.Random.Strict as X (Random)
import Data.Aeson as X
import qualified Data.Char as C
import Data.Coerce as X (coerce)
import Data.List as X (nub, (\\))
import Data.UUID as X (UUID)
import GHC.Stack as X
import Language.Haskell.TH hiding (location)
import Safe as X (fromJustNote)
import System.Random.Shuffle as X

suffixedNamer :: FieldNamer
suffixedNamer _ _ n = case dropWhile C.isLower (nameBase n) of
  x : xs -> [TopName (mkName ((C.toLower x : xs) ++ "L"))]
  _ -> []

suffixedWithNamer :: String -> FieldNamer
suffixedWithNamer str _ _ n = case drop (length str) (nameBase n) of
  x : xs -> [TopName (mkName ((C.toLower x : xs) ++ "L"))]
  _ -> []

suffixedWithFields :: String -> LensRules
suffixedWithFields suffix =
  defaultFieldRules & lensField .~ suffixedWithNamer suffix

suffixedFields :: LensRules
suffixedFields = defaultFieldRules & lensField .~ suffixedNamer
