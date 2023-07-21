module Arkham.Asset.Cards.DeathXIII1 (
  deathXiii1,
  DeathXIII1 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Window (defaultWindows)

newtype DeathXIII1 = DeathXIII1 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deathXiii1 :: AssetCard DeathXIII1
deathXiii1 =
  asset DeathXIII1 Cards.deathXiii1

instance HasModifiersFor DeathXIII1 where
  getModifiersFor (InvestigatorTarget iid) (DeathXIII1 a) =
    pure $
      toModifiers a [SkillModifier SkillIntellect 1 | controlledBy a iid]
  getModifiersFor _ _ = pure []

instance HasAbilities DeathXIII1 where
  getAbilities (DeathXIII1 a) =
    [restrictedAbility a 1 InYourHand $ ReactionAbility (GameBegins Timing.When) Free]

instance RunMessage DeathXIII1 where
  runMessage msg a@(DeathXIII1 attrs) = case msg of
    InHand _ (UseCardAbility iid (isSource attrs -> True) 1 _ _) -> do
      push (PutCardIntoPlay iid (toCard attrs) Nothing (defaultWindows iid))
      pure a
    _ -> DeathXIII1 <$> runMessage msg attrs
