module Arkham.Asset.Assets.FiveOfPentacles1 (fiveOfPentacles1, FiveOfPentacles1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype FiveOfPentacles1 = FiveOfPentacles1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fiveOfPentacles1 :: AssetCard FiveOfPentacles1
fiveOfPentacles1 = asset FiveOfPentacles1 Cards.fiveOfPentacles1

instance HasModifiersFor FiveOfPentacles1 where
  getModifiersFor (FiveOfPentacles1 a) = controllerGets a [HealthModifier 1, SanityModifier 1]

instance HasAbilities FiveOfPentacles1 where
  getAbilities (FiveOfPentacles1 a) =
    [restrictedAbility a 1 InYourHand $ freeReaction (GameBegins #when)]

instance RunMessage FiveOfPentacles1 where
  runMessage msg a@(FiveOfPentacles1 attrs) = case msg of
    InHand _ (UseThisAbility iid (isSource attrs -> True) 1) -> do
      push $ putCardIntoPlay iid attrs
      pure a
    _ -> FiveOfPentacles1 <$> runMessage msg attrs
