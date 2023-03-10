module Arkham.Act.Cards.WitchHauntings
  ( WitchHauntings(..)
  , witchHauntings
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype WitchHauntings = WitchHauntings ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

witchHauntings :: ActCard WitchHauntings
witchHauntings = act (2, A) WitchHauntings Cards.witchHauntings Nothing

instance RunMessage WitchHauntings where
  runMessage msg (WitchHauntings attrs) = WitchHauntings <$> runMessage msg attrs
