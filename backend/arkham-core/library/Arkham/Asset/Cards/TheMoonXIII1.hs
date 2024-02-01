module Arkham.Asset.Cards.TheMoonXIII1 (
  theMoonXiii1,
  TheMoonXIII1 (..),
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

newtype TheMoonXIII1 = TheMoonXIII1 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

theMoonXiii1 :: AssetCard TheMoonXIII1
theMoonXiii1 =
  asset TheMoonXIII1 Cards.theMoonXiii1

instance HasModifiersFor TheMoonXIII1 where
  getModifiersFor (InvestigatorTarget iid) (TheMoonXIII1 a) =
    pure
      $ toModifiers a [SkillModifier SkillAgility 1 | controlledBy a iid]
  getModifiersFor _ _ = pure []

instance HasAbilities TheMoonXIII1 where
  getAbilities (TheMoonXIII1 a) =
    [restrictedAbility a 1 InYourHand $ ReactionAbility (GameBegins Timing.When) Free]

instance RunMessage TheMoonXIII1 where
  runMessage msg a@(TheMoonXIII1 attrs) = case msg of
    InHand _ (UseCardAbility iid (isSource attrs -> True) 1 _ _) -> do
      push (PutCardIntoPlay iid (toCard attrs) Nothing (defaultWindows iid))
      pure a
    _ -> TheMoonXIII1 <$> runMessage msg attrs
