module Arkham.Asset.Assets.MariaDeSilva (mariaDeSilva) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted

newtype MariaDeSilva = MariaDeSilva AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mariaDeSilva :: AssetCard MariaDeSilva
mariaDeSilva = asset MariaDeSilva Cards.mariaDeSilva

instance HasAbilities MariaDeSilva where
  getAbilities (MariaDeSilva a) =
    [skillTestAbility $ restricted a 1 OnSameLocation $ parleyAction (ResourceCost 1)]

instance RunMessage MariaDeSilva where
  runMessage msg a@(MariaDeSilva attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #intellect (Fixed 3)
      pure a
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      placeClues (attrs.ability 1) attrs 1
      pure a
    _ -> MariaDeSilva <$> liftRunMessage msg attrs
