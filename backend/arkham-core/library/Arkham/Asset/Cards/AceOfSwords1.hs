module Arkham.Asset.Cards.AceOfSwords1 (aceOfSwords1, AceOfSwords1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype AceOfSwords1 = AceOfSwords1 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aceOfSwords1 :: AssetCard AceOfSwords1
aceOfSwords1 = asset AceOfSwords1 Cards.aceOfSwords1

instance HasModifiersFor AceOfSwords1 where
  getModifiersFor (InvestigatorTarget iid) (AceOfSwords1 a) =
    pure $ toModifiers a [SkillModifier #combat 1 | controlledBy a iid]
  getModifiersFor _ _ = pure []

instance HasAbilities AceOfSwords1 where
  getAbilities (AceOfSwords1 a) = [reactionAbility a 1 Free (GameBegins #when) InYourHand]

instance RunMessage AceOfSwords1 where
  runMessage msg a@(AceOfSwords1 attrs) = case msg of
    InHand _ (UseThisAbility iid (isSource attrs -> True) 1) -> do
      push $ putCardIntoPlay iid attrs
      pure a
    _ -> AceOfSwords1 <$> runMessage msg attrs
