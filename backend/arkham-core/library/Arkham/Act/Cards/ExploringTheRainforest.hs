module Arkham.Act.Cards.ExploringTheRainforest
  ( ExploringTheRainforest(..)
  , exploringTheRainforest
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype ExploringTheRainforest = ExploringTheRainforest ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exploringTheRainforest :: ActCard ExploringTheRainforest
exploringTheRainforest = act (1, A) ExploringTheRainforest Cards.exploringTheRainforest Nothing

instance RunMessage ExploringTheRainforest where
  runMessage msg (ExploringTheRainforest attrs) = ExploringTheRainforest <$> runMessage msg attrs
