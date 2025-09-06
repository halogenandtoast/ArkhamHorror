module Arkham.Asset.Assets.DeathXIII1 (deathXiii1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Prelude

newtype DeathXIII1 = DeathXIII1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deathXiii1 :: AssetCard DeathXIII1
deathXiii1 = asset DeathXIII1 Cards.deathXiii1

instance HasModifiersFor DeathXIII1 where
  getModifiersFor (DeathXIII1 a) = controllerGets a [SkillModifier #intellect 1]

instance HasAbilities DeathXIII1 where
  getAbilities (DeathXIII1 a) = [restrictedAbility a 1 InYourHand $ freeReaction (GameBegins #when)]

instance RunMessage DeathXIII1 where
  runMessage msg a@(DeathXIII1 attrs) = case msg of
    InHand iid (UseCardAbility iid' (isSource attrs -> True) 1 _ _) | iid == iid' -> do
      push $ putCardIntoPlay iid attrs
      pure a
    _ -> DeathXIII1 <$> runMessage msg attrs
