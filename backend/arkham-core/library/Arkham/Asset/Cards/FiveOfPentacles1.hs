module Arkham.Asset.Cards.FiveOfPentacles1 (
  fiveOfPentacles1,
  FiveOfPentacles1 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Window (defaultWindows)

newtype FiveOfPentacles1 = FiveOfPentacles1 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fiveOfPentacles1 :: AssetCard FiveOfPentacles1
fiveOfPentacles1 =
  asset FiveOfPentacles1 Cards.fiveOfPentacles1

instance HasModifiersFor FiveOfPentacles1 where
  getModifiersFor (InvestigatorTarget iid) (FiveOfPentacles1 a)
    | controlledBy a iid =
        pure
          $ toModifiers a [HealthModifier 1, SanityModifier 1]
  getModifiersFor _ _ = pure []

instance HasAbilities FiveOfPentacles1 where
  getAbilities (FiveOfPentacles1 a) =
    [restrictedAbility a 1 InYourHand $ ReactionAbility (GameBegins Timing.When) Free]

instance RunMessage FiveOfPentacles1 where
  runMessage msg a@(FiveOfPentacles1 attrs) = case msg of
    InHand _ (UseCardAbility iid (isSource attrs -> True) 1 _ _) -> do
      push $ putCardIntoPlay iid attrs
      pure a
    _ -> FiveOfPentacles1 <$> runMessage msg attrs
