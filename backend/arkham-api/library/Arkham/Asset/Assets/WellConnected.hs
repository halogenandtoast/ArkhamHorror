module Arkham.Asset.Assets.WellConnected (wellConnected) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Modifier
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher

newtype WellConnected = WellConnected AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wellConnected :: AssetCard WellConnected
wellConnected = asset WellConnected Cards.wellConnected

instance HasAbilities WellConnected where
  getAbilities (WellConnected a) =
    [ wantsSkillTest (YourSkillTest #any)
        $ controlled a 1 (DuringSkillTest #any)
        $ FastAbility (exhaust a)
    ]

instance RunMessage WellConnected where
  runMessage msg a@(WellConnected attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        skillTestModifier sid (attrs.ability 1) iid
          $ AnySkillValueCalculated
          $ DividedByCalculation (InvestigatorFieldCalculation iid #resources) 5
      pure a
    _ -> WellConnected <$> liftRunMessage msg attrs
