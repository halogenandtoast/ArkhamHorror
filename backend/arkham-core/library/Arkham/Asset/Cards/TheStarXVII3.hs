module Arkham.Asset.Cards.TheStarXVII3 (
  theStarXvii3,
  TheStarXVII3 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher
import Arkham.Window (defaultWindows)

newtype TheStarXVII3 = TheStarXVII3 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStarXvii3 :: AssetCard TheStarXVII3
theStarXvii3 = asset TheStarXVII3 Cards.theStarXvii3

instance HasModifiersFor TheStarXVII3 where
  getModifiersFor (AssetTarget aid) (TheStarXVII3 attrs) = case assetController attrs of
    Nothing -> pure []
    Just iid -> do
      isHealthAsset <- aid <=~> (assetControlledBy iid <> AssetWithHealth)
      isSanityAsset <- aid <=~> (assetControlledBy iid <> AssetWithSanity)
      pure
        $ toModifiers attrs
        $ [HealthModifier 1 | isHealthAsset] <> [SanityModifier 1 | isSanityAsset]
  getModifiersFor _ _ = pure []

instance HasAbilities TheStarXVII3 where
  getAbilities (TheStarXVII3 a) = [restrictedAbility a 1 InYourHand $ freeReaction (GameBegins #when)]

instance RunMessage TheStarXVII3 where
  runMessage msg a@(TheStarXVII3 attrs) = case msg of
    InHand _ (UseCardAbility iid (isSource attrs -> True) 1 _ _) -> do
      push $ PutCardIntoPlay iid (toCard attrs) Nothing (defaultWindows iid)
      pure a
    _ -> TheStarXVII3 <$> runMessage msg attrs
