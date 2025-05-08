module Arkham.Location.Cards.DimensionalGap (dimensionalGap) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (dimensionalGap)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype DimensionalGap = DimensionalGap LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimensionalGap :: LocationCard DimensionalGap
dimensionalGap = location DimensionalGap Cards.dimensionalGap 3 (PerPlayer 1)

instance HasAbilities DimensionalGap where
  getAbilities (DimensionalGap a) = extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after You (be a)

instance RunMessage DimensionalGap where
  runMessage msg l@(DimensionalGap attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardUntilFirst iid (attrs.ability 1) Deck.EncounterDeck #enemy
      pure l
    RequestedEncounterCard (isSource attrs -> True) _ (Just ec) -> do
      spawnEnemyAt_ ec attrs
      pure l
    _ -> DimensionalGap <$> liftRunMessage msg attrs
