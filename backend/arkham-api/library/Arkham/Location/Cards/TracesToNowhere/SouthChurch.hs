module Arkham.Location.Cards.TracesToNowhere.SouthChurch (southChurch) where

import Arkham.Ability
import Arkham.Location.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Location.Import.Lifted

newtype SouthChurch = SouthChurch LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

southChurch :: LocationCard SouthChurch
southChurch = location SouthChurch Cards.southChurch 3 (PerPlayer 1)

instance HasAbilities SouthChurch where
  getAbilities (SouthChurch a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 Here doubleActionAbility

instance RunMessage SouthChurch where
  runMessage msg l@(SouthChurch attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      healHorror iid (attrs.ability 1) 2
      drawCards iid (attrs.ability 1) 2
      pure l
    _ -> SouthChurch <$> liftRunMessage msg attrs
