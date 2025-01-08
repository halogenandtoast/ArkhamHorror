module Arkham.Asset.Assets.TheStarXVII3 (theStarXvii3, TheStarXVII3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype TheStarXVII3 = TheStarXVII3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStarXvii3 :: AssetCard TheStarXVII3
theStarXvii3 = asset TheStarXVII3 Cards.theStarXvii3

instance HasModifiersFor TheStarXVII3 where
  getModifiersFor (TheStarXVII3 a) = for_ a.controller \iid -> do
    modifySelect a (assetControlledBy iid <> AssetWithHealth) [HealthModifier 1]
    modifySelect a (assetControlledBy iid <> AssetWithSanity) [SanityModifier 1]

instance HasAbilities TheStarXVII3 where
  getAbilities (TheStarXVII3 a) = [restrictedAbility a 1 InYourHand $ freeReaction (GameBegins #when)]

instance RunMessage TheStarXVII3 where
  runMessage msg a@(TheStarXVII3 attrs) = case msg of
    InHand _ (UseCardAbility iid (isSource attrs -> True) 1 _ _) -> do
      push $ putCardIntoPlay iid attrs
      pure a
    _ -> TheStarXVII3 <$> runMessage msg attrs
