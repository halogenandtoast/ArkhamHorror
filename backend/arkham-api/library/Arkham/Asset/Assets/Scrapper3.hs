module Arkham.Asset.Assets.Scrapper3 (scrapper3, Scrapper3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype Scrapper3 = Scrapper3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scrapper3 :: AssetCard Scrapper3
scrapper3 = asset Scrapper3 Cards.scrapper3

instance HasAbilities Scrapper3 where
  getAbilities (Scrapper3 a) =
    [ withTooltip "{fast} Spend 1 resource: You get +1 {combat} for this skill test."
        $ wantsSkillTest (YourSkillTest #combat)
        $ controlledAbility a 1 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    , withTooltip "{fast} Spend 1 resource: You get +1 {agility} for this skill test."
        $ wantsSkillTest (YourSkillTest #agility)
        $ controlledAbility a 2 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    ]

instance RunMessage Scrapper3 where
  runMessage msg a@(Scrapper3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      phaseModifier (attrs.ability 1) iid $ SkillModifier #combat 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      phaseModifier (attrs.ability 2) iid $ SkillModifier #agility 1
      pure a
    _ -> Scrapper3 <$> liftRunMessage msg attrs
