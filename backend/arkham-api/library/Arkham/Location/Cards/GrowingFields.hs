module Arkham.Location.Cards.GrowingFields (growingFields) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Modifiers (ModifierType (..), modifyEach)
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype GrowingFields = GrowingFields LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

growingFields :: LocationCard GrowingFields
growingFields = location GrowingFields Cards.growingFields 3 (Static 1)

instance HasModifiersFor GrowingFields where
  getModifiersFor (GrowingFields a) =
    -- During its own test, double the skill icons of each card committed to the test.
    whenJustM getSkillTest \st ->
      when (isAbilitySource a 1 st.source)
        $ modifyEach a (concat $ toList st.committedCards) [DoubleSkillIcons]

instance HasAbilities GrowingFields where
  getAbilities (GrowingFields a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage GrowingFields where
  runMessage msg l@(GrowingFields attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      placeSetAsideLocation_ Cards.hiddenVault
      GrowingFields <$> liftRunMessage msg attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 8)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      takeControlOfSetAsideAsset iid =<< getSetAsideCard Assets.grislyMask
      pure l
    _ -> GrowingFields <$> liftRunMessage msg attrs
