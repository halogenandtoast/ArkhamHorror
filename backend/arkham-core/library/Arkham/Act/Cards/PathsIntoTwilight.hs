module Arkham.Act.Cards.PathsIntoTwilight
  ( PathsIntoTwilight(..)
  , pathsIntoTwilight
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype PathsIntoTwilight = PathsIntoTwilight ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

pathsIntoTwilight :: ActCard PathsIntoTwilight
pathsIntoTwilight = act (3, A) PathsIntoTwilight Cards.pathsIntoTwilight Nothing

instance RunMessage PathsIntoTwilight where
  runMessage msg (PathsIntoTwilight attrs) = PathsIntoTwilight <$> runMessage msg attrs
