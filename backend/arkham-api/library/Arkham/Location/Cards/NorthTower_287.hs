module Arkham.Location.Cards.NorthTower_287 (northTower_287, NorthTower_287 (..)) where

import Arkham.Ability
import Arkham.Agenda.Sequence (AgendaSide (C))
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype NorthTower_287 = NorthTower_287 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

northTower_287 :: LocationCard NorthTower_287
northTower_287 = location NorthTower_287 Cards.northTower_287 2 (PerPlayer 2)

instance HasAbilities NorthTower_287 where
  getAbilities (NorthTower_287 a) =
    extendRevealed1 a
      $ restrictedAbility a 1 Here
      $ forced
      $ PlacedCounterOnAgenda #after (AgendaWithSide C) AnySource DoomCounter (atLeast 1)

instance RunMessage NorthTower_287 where
  runMessage msg l@(NorthTower_287 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (investigatorAt attrs) \iid -> assignHorror iid (attrs.ability 1) 1
      pure l
    _ -> NorthTower_287 <$> liftRunMessage msg attrs
