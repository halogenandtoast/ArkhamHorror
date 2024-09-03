module Arkham.Asset.Cards.TheMoonXIII1 (theMoonXiii1, TheMoonXIII1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype TheMoonXIII1 = TheMoonXIII1 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMoonXiii1 :: AssetCard TheMoonXIII1
theMoonXiii1 = asset TheMoonXIII1 Cards.theMoonXiii1

instance HasModifiersFor TheMoonXIII1 where
  getModifiersFor (InvestigatorTarget iid) (TheMoonXIII1 a) = do
    pure $ toModifiers a [SkillModifier #agility 1 | controlledBy a iid]
  getModifiersFor _ _ = pure []

instance HasAbilities TheMoonXIII1 where
  getAbilities (TheMoonXIII1 a) = [restrictedAbility a 1 InYourHand $ freeReaction (GameBegins #when)]

instance RunMessage TheMoonXIII1 where
  runMessage msg a@(TheMoonXIII1 attrs) = case msg of
    InHand _ (UseCardAbility iid (isSource attrs -> True) 1 _ _) -> do
      push $ putCardIntoPlay iid attrs
      pure a
    _ -> TheMoonXIII1 <$> runMessage msg attrs
