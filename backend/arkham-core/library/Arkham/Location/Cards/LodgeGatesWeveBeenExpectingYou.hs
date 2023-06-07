module Arkham.Location.Cards.LodgeGatesWeveBeenExpectingYou
  ( lodgeGatesWeveBeenExpectingYou
  , LodgeGatesWeveBeenExpectingYou(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype LodgeGatesWeveBeenExpectingYou = LodgeGatesWeveBeenExpectingYou LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lodgeGatesWeveBeenExpectingYou :: LocationCard LodgeGatesWeveBeenExpectingYou
lodgeGatesWeveBeenExpectingYou = location LodgeGatesWeveBeenExpectingYou Cards.lodgeGatesWeveBeenExpectingYou 2 (Static 0)

instance HasAbilities LodgeGatesWeveBeenExpectingYou where
  getAbilities (LodgeGatesWeveBeenExpectingYou attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage LodgeGatesWeveBeenExpectingYou where
  runMessage msg (LodgeGatesWeveBeenExpectingYou attrs) =
    LodgeGatesWeveBeenExpectingYou <$> runMessage msg attrs
