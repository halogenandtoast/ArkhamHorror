module Arkham.Location.Cards.DeepBelowYourHouse (deepBelowYourHouse) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (deepBelowYourHouse)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype DeepBelowYourHouse = DeepBelowYourHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepBelowYourHouse :: LocationCard DeepBelowYourHouse
deepBelowYourHouse = location DeepBelowYourHouse Cards.deepBelowYourHouse 4 (PerPlayer 1)

instance HasAbilities DeepBelowYourHouse where
  getAbilities (DeepBelowYourHouse a) =
    extendRevealed1 a $ skillTestAbility $ mkAbility a 1 $ forced $ RevealLocation #after You (be a)

instance RunMessage DeepBelowYourHouse where
  runMessage msg l@(DeepBelowYourHouse attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 3)
      pure l
    FailedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      repeated n $ findAndDrawEncounterCard iid $ cardIs Enemies.swarmOfRats
      pure l
    _ -> DeepBelowYourHouse <$> liftRunMessage msg attrs
