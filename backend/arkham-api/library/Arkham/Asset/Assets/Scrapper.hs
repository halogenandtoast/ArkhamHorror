module Arkham.Asset.Assets.Scrapper (scrapper, Scrapper (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype Scrapper = Scrapper AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scrapper :: AssetCard Scrapper
scrapper = asset Scrapper Cards.scrapper

instance HasAbilities Scrapper where
  getAbilities (Scrapper a) =
    [ withTooltip "{fast} Spend 1 resource: You get +1 {combat} for this skill test."
        $ wantsSkillTest (YourSkillTest #combat)
        $ controlledAbility a 1 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    , withTooltip "{fast} Spend 1 resource: You get +1 {agility} for this skill test."
        $ wantsSkillTest (YourSkillTest #agility)
        $ controlledAbility a 2 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    ]

instance RunMessage Scrapper where
  runMessage msg a@(Scrapper attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      phaseModifier (attrs.ability 1) iid $ SkillModifier #combat 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      phaseModifier (attrs.ability 2) iid $ SkillModifier #agility 1
      pure a
    _ -> Scrapper <$> liftRunMessage msg attrs
