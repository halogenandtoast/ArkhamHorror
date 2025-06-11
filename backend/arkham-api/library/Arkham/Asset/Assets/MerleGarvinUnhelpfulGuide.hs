module Arkham.Asset.Assets.MerleGarvinUnhelpfulGuide (merleGarvinUnhelpfulGuide) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement

newtype MerleGarvinUnhelpfulGuide = MerleGarvinUnhelpfulGuide AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

merleGarvinUnhelpfulGuide :: AssetCard MerleGarvinUnhelpfulGuide
merleGarvinUnhelpfulGuide = asset MerleGarvinUnhelpfulGuide Cards.merleGarvinUnhelpfulGuide

instance HasAbilities MerleGarvinUnhelpfulGuide where
  getAbilities (MerleGarvinUnhelpfulGuide a) =
    [skillTestAbility $ restricted a 1 OnSameLocation parleyAction_]

instance RunMessage MerleGarvinUnhelpfulGuide where
  runMessage msg a@(MerleGarvinUnhelpfulGuide attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #agility (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      locations <- select $ LocationWithDistanceFrom 1 (locationWithInvestigator iid) Anywhere
      chooseTargetM iid locations $ place attrs . AtLocation
      placeClues (attrs.ability 1) attrs 1
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      locations <- select $ LocationWithDistanceFrom 2 (locationWithInvestigator iid) Anywhere
      chooseTargetM iid locations $ place attrs . AtLocation
      pure a
    _ -> MerleGarvinUnhelpfulGuide <$> liftRunMessage msg attrs
