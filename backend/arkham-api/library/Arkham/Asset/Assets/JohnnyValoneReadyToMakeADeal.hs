module Arkham.Asset.Assets.JohnnyValoneReadyToMakeADeal (
  johnnyValoneReadyToMakeADeal,
  JohnnyValoneReadyToMakeADeal(..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher

newtype JohnnyValoneReadyToMakeADeal = JohnnyValoneReadyToMakeADeal AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

johnnyValoneReadyToMakeADeal :: AssetCard JohnnyValoneReadyToMakeADeal
johnnyValoneReadyToMakeADeal =
  allyWith JohnnyValoneReadyToMakeADeal Cards.johnnyValoneReadyToMakeADeal (3, 2) noSlots

instance HasModifiersFor JohnnyValoneReadyToMakeADeal where
  getModifiersFor (JohnnyValoneReadyToMakeADeal a) = controllerGets a [SkillModifier #agility 1]

instance HasAbilities JohnnyValoneReadyToMakeADeal where
  getAbilities (JohnnyValoneReadyToMakeADeal a) =
    [ reaction a 1 ControlsThis (exhaust a) $
        GainsResources #after You SourceIsCardEffect (atLeast 1)
    , mkAbility a 2 $ forced $ AssetLeavesPlay #when (be a)
    ]

instance RunMessage JohnnyValoneReadyToMakeADeal where
  runMessage msg a@(JohnnyValoneReadyToMakeADeal attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      drawCards iid (attrs.ability 1) 1
      gainResourcesIfCan iid (attrs.ability 1) 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      removeFromGame attrs
      pure a
    _ -> JohnnyValoneReadyToMakeADeal <$> liftRunMessage msg attrs
