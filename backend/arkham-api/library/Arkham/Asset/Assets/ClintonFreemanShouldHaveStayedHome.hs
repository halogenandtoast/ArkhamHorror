module Arkham.Asset.Assets.ClintonFreemanShouldHaveStayedHome (clintonFreemanShouldHaveStayedHome) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Message.Lifted.Choose

newtype ClintonFreemanShouldHaveStayedHome = ClintonFreemanShouldHaveStayedHome AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clintonFreemanShouldHaveStayedHome :: AssetCard ClintonFreemanShouldHaveStayedHome
clintonFreemanShouldHaveStayedHome = asset ClintonFreemanShouldHaveStayedHome Cards.clintonFreemanShouldHaveStayedHome

instance HasAbilities ClintonFreemanShouldHaveStayedHome where
  getAbilities (ClintonFreemanShouldHaveStayedHome a) =
    [skillTestAbility $ restricted a 1 OnSameLocation parleyAction_]

instance RunMessage ClintonFreemanShouldHaveStayedHome where
  runMessage msg a@(ClintonFreemanShouldHaveStayedHome attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#willpower, #intellect] \kind ->
          skillLabeled kind $ parley sid iid (attrs.ability 1) attrs kind (Fixed 3)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      placeClues (attrs.ability 1) attrs 1
      assignHorror iid (attrs.ability 1) 1
      pure a
    FailedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      assignHorror iid (attrs.ability 1) n
      pure a
    _ -> ClintonFreemanShouldHaveStayedHome <$> liftRunMessage msg attrs
