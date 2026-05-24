module Arkham.Location.Cards.AshenSlope (ashenSlope) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype AshenSlope = AshenSlope LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ashenSlope :: LocationCard AshenSlope
ashenSlope = symbolLabel $ locationWith AshenSlope Cards.ashenSlope 4 (PerPlayer 1) connectsToAdjacent

instance HasAbilities AshenSlope where
  getAbilities (AshenSlope a) =
    extendRevealed1 a
      $ skillTestAbility
      $ mkAbility a 1
      $ forced
      $ RevealLocation #after You (be a)

instance RunMessage AshenSlope where
  runMessage msg l@(AshenSlope attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 4)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignDamage iid (attrs.ability 1) 2
      pure l
    _ -> AshenSlope <$> liftRunMessage msg attrs
