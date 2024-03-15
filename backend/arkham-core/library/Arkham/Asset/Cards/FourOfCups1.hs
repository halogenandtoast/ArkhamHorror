module Arkham.Asset.Cards.FourOfCups1 (fourOfCups1, FourOfCups1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype FourOfCups1 = FourOfCups1 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fourOfCups1 :: AssetCard FourOfCups1
fourOfCups1 = asset FourOfCups1 Cards.fourOfCups1

instance HasModifiersFor FourOfCups1 where
  getModifiersFor (InvestigatorTarget iid) (FourOfCups1 a) = do
    pure $ toModifiers a [SkillModifier #willpower 1 | controlledBy a iid]
  getModifiersFor _ _ = pure []

instance HasAbilities FourOfCups1 where
  getAbilities (FourOfCups1 a) =
    [restrictedAbility a 1 InYourHand $ freeReaction (GameBegins #when)]

instance RunMessage FourOfCups1 where
  runMessage msg a@(FourOfCups1 attrs) = case msg of
    InHand _ (UseThisAbility iid (isSource attrs -> True) 1) -> do
      push $ putCardIntoPlay iid attrs
      pure a
    _ -> FourOfCups1 <$> runMessage msg attrs
