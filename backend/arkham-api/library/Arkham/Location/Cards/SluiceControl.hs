module Arkham.Location.Cards.SluiceControl (sluiceControl) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Message (pattern R3)
import Arkham.Location.Cards qualified as Cards (sluiceControl)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose (chooseBeginSkillTest)

newtype SluiceControl = SluiceControl LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sluiceControl :: LocationCard SluiceControl
sluiceControl = location SluiceControl Cards.sluiceControl 4 (PerPlayer 2)

instance HasModifiersFor SluiceControl where
  getModifiersFor _ = pure mempty

instance HasAbilities SluiceControl where
  getAbilities (SluiceControl a) =
    extendRevealed
      a
      [ skillTestAbility $ restricted a 1 Here actionAbility
      ]

instance RunMessage SluiceControl where
  runMessage msg l@(SluiceControl attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) iid [#combat, #agility] (Fixed 5)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      enemies <- select AnyEnemy
      for_ enemies \eid -> defeatEnemy eid iid (attrs.ability 1)
      push R3
      pure l
    _ -> SluiceControl <$> liftRunMessage msg attrs
