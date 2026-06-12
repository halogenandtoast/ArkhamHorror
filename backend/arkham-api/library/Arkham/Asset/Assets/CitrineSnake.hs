module Arkham.Asset.Assets.CitrineSnake (citrineSnake) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (InvestigatorResigned)
import Arkham.Campaigns.TheForgottenAge.Helpers (becomePoisonedOr)
import Arkham.Matcher

newtype CitrineSnake = CitrineSnake AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

citrineSnake :: AssetCard CitrineSnake
citrineSnake = asset CitrineSnake Cards.citrineSnake

instance HasAbilities CitrineSnake where
  getAbilities (CitrineSnake a) =
    [restricted a 1 ControlsThis $ forced $ InvestigatorResigned #when You]

instance RunMessage CitrineSnake where
  runMessage msg a@(CitrineSnake attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      putCardIntoPlay iid attrs
      becomePoisonedOr iid $ directDamage iid attrs 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      addToVictory iid attrs
      pure a
    _ -> CitrineSnake <$> liftRunMessage msg attrs
