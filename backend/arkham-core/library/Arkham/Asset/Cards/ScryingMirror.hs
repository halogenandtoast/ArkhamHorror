module Arkham.Asset.Cards.ScryingMirror (scryingMirror, ScryingMirror (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype ScryingMirror = ScryingMirror AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scryingMirror :: AssetCard ScryingMirror
scryingMirror = asset ScryingMirror Cards.scryingMirror

instance HasAbilities ScryingMirror where
  getAbilities (ScryingMirror a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (InitiatedSkillTest #when (InvestigatorAt YourLocation) AnySkillType AnySkillTestValue #any)
          (exhaust a <> assetUseCost a Secret 1)
    ]

instance RunMessage ScryingMirror where
  runMessage msg a@(ScryingMirror attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      withSkillTest \sid ->
        push $ skillTestModifier sid attrs sid RevealChaosTokensBeforeCommittingCards
      pure a
    _ -> ScryingMirror <$> runMessage msg attrs
