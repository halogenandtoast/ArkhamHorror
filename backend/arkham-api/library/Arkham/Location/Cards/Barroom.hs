module Arkham.Location.Cards.Barroom (barroom) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype Barroom = Barroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | 'Barroom' from The Midwinter Gala (#71011).
barroom :: LocationCard Barroom
barroom = location Barroom Cards.barroom 3 (PerPlayer 2)

instance HasAbilities Barroom where
  getAbilities (Barroom a) =
    extendRevealed1 a $ groupLimit PerRound $ restricted a 1 Here $ FastAbility (ResourceCost 1)

instance RunMessage Barroom where
  runMessage msg l@(Barroom attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- TODO: Implement heal ability
      pure l
    _ -> Barroom <$> runMessage msg attrs
