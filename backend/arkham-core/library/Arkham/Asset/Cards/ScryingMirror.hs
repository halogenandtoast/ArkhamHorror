module Arkham.Asset.Cards.ScryingMirror (
  scryingMirror,
  ScryingMirror (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype ScryingMirror = ScryingMirror AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scryingMirror :: AssetCard ScryingMirror
scryingMirror = asset ScryingMirror Cards.scryingMirror

instance HasAbilities ScryingMirror where
  getAbilities (ScryingMirror a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (InitiatedSkillTest Timing.When (InvestigatorAt YourLocation) AnySkillType AnySkillTestValue #any)
        $ ExhaustCost (toTarget a)
        <> UseCost (AssetWithId $ toId a) Secret 1
    ]

instance RunMessage ScryingMirror where
  runMessage msg a@(ScryingMirror attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ skillTestModifier attrs SkillTestTarget RevealChaosTokensBeforeCommittingCards
      pure a
    _ -> ScryingMirror <$> runMessage msg attrs
