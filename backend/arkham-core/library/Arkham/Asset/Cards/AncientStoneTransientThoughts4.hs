module Arkham.Asset.Cards.AncientStoneTransientThoughts4 (
  ancientStoneTransientThoughts4,
  AncientStoneTransientThoughts4 (..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Movement

newtype AncientStoneTransientThoughts4 = AncientStoneTransientThoughts4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientStoneTransientThoughts4
  :: AssetCard AncientStoneTransientThoughts4
ancientStoneTransientThoughts4 = asset AncientStoneTransientThoughts4 Cards.ancientStoneTransientThoughts4

instance HasAbilities AncientStoneTransientThoughts4 where
  getAbilities (AncientStoneTransientThoughts4 a) =
    [ controlledAbility a 1 (youExist can.move)
        $ ReactionAbility (DrawsCards #when You AnyValue) (DynamicUseCost (be a) Secret DrawnCardsValue)
    ]

instance RunMessage AncientStoneTransientThoughts4 where
  runMessage msg a@(AncientStoneTransientThoughts4 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 ws p@(totalUsesPayment -> n) -> do
      pushAll $ replicate n $ UseCardAbilityStep iid (toSource attrs) 1 ws p 1
      pure a
    UseCardAbilityStep iid (isSource attrs -> True) 1 _ _ 1 -> do
      selectWhenNotNull (AccessibleFrom $ locationWithInvestigator iid) \targets ->
        chooseOne iid $ targetLabels targets (only . Move . move (attrs.ability 1) iid)
      pure a
    _ -> AncientStoneTransientThoughts4 <$> liftRunMessage msg attrs
