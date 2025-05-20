module Arkham.Asset.Assets.NoseToTheGrindstone3 (noseToTheGrindstone3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Helpers.SkillTest (getSkillTestSource, withSkillTest)
import Arkham.Helpers.Use (toStartingUses)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection
import Arkham.Trait (Trait (Tool))

newtype NoseToTheGrindstone3 = NoseToTheGrindstone3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

noseToTheGrindstone3 :: AssetCard NoseToTheGrindstone3
noseToTheGrindstone3 = asset NoseToTheGrindstone3 Cards.noseToTheGrindstone3

instance HasAbilities NoseToTheGrindstone3 where
  getAbilities (NoseToTheGrindstone3 x) =
    [ controlled x 1 (DuringSkillTest $ YourSkillTest $ SkillTestOnAsset $ AssetWithTrait Tool)
        $ FastAbility (exhaust x)
    ]

instance RunMessage NoseToTheGrindstone3 where
  runMessage msg a@(NoseToTheGrindstone3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 2)
        onFailedByEffect sid (atLeast 0) (attrs.ability 1) sid $ doStep 1 msg
      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      mAsset <- runMaybeT do
        s <- MaybeT getSkillTestSource
        aid <- hoistMaybe s.asset
        (uType, n) <-
          MaybeT $ fmap (listToMaybe . mapToList) . toStartingUses =<< field AssetStartingUses aid
        current <- lift $ findWithDefault 0 uType <$> field AssetUses aid
        guard $ uType == Supply && current < n
        pure aid

      chooseOneM iid do
        for_ mAsset \aid ->
          labeled "Replenish 1 supply on that asset" $ addUses (attrs.ability 1) aid Supply 1
        whenM (can.gain.resources iid) do
          labeled "Gain 1 resource" $ gainResources iid (attrs.ability 1) 1
      pure a
    _ -> NoseToTheGrindstone3 <$> liftRunMessage msg attrs
