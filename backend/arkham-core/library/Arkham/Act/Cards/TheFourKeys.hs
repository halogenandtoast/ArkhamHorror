module Arkham.Act.Cards.TheFourKeys
  ( TheFourKeys(..)
  , theFourKeys
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype TheFourKeys = TheFourKeys ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theFourKeys :: ActCard TheFourKeys
theFourKeys = act (3, A) TheFourKeys Cards.theFourKeys Nothing

instance RunMessage TheFourKeys where
  runMessage msg (TheFourKeys attrs) = TheFourKeys <$> runMessage msg attrs
