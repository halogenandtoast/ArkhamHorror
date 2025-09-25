module Arkham.Location.Cards.BalconyAtDeathsDoorstep (balconyAtDeathsDoorstep) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype BalconyAtDeathsDoorstep = BalconyAtDeathsDoorstep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

balconyAtDeathsDoorstep :: LocationCard BalconyAtDeathsDoorstep
balconyAtDeathsDoorstep =
  location BalconyAtDeathsDoorstep Cards.balconyAtDeathsDoorstep 1 (Static 0)

instance HasAbilities BalconyAtDeathsDoorstep where
  getAbilities (BalconyAtDeathsDoorstep a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 Here
      $ parleyAction
      $ SkillIconCost 3 (singleton #intellect)

instance RunMessage BalconyAtDeathsDoorstep where
  runMessage msg l@(BalconyAtDeathsDoorstep attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainClues iid (attrs.ability 1) 2
      pure l
    _ -> BalconyAtDeathsDoorstep <$> liftRunMessage msg attrs
