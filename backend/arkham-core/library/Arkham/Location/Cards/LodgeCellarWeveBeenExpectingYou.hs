module Arkham.Location.Cards.LodgeCellarWeveBeenExpectingYou
  ( lodgeCellarWeveBeenExpectingYou
  , LodgeCellarWeveBeenExpectingYou(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype LodgeCellarWeveBeenExpectingYou = LodgeCellarWeveBeenExpectingYou LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lodgeCellarWeveBeenExpectingYou :: LocationCard LodgeCellarWeveBeenExpectingYou
lodgeCellarWeveBeenExpectingYou = location LodgeCellarWeveBeenExpectingYou Cards.lodgeCellarWeveBeenExpectingYou 3 (PerPlayer 1)

instance HasAbilities LodgeCellarWeveBeenExpectingYou where
  getAbilities (LodgeCellarWeveBeenExpectingYou attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage LodgeCellarWeveBeenExpectingYou where
  runMessage msg (LodgeCellarWeveBeenExpectingYou attrs) =
    LodgeCellarWeveBeenExpectingYou <$> runMessage msg attrs
