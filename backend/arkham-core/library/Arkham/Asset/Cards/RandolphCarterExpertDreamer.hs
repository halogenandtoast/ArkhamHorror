module Arkham.Asset.Cards.RandolphCarterExpertDreamer (
  randolphCarterExpertDreamer,
  RandolphCarterExpertDreamer (..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Capability
import Arkham.ChaosToken
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Window qualified as Window

newtype RandolphCarterExpertDreamer = RandolphCarterExpertDreamer AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

randolphCarterExpertDreamer :: AssetCard RandolphCarterExpertDreamer
randolphCarterExpertDreamer = ally RandolphCarterExpertDreamer Cards.randolphCarterExpertDreamer (3, 2)

instance HasModifiersFor RandolphCarterExpertDreamer where
  getModifiersFor (InvestigatorTarget iid) (RandolphCarterExpertDreamer a) | a `controlledBy` iid = do
    pure $ toModifiers a [SkillModifier #combat 1, SkillModifier #agility 1]
  getModifiersFor _ _ = pure []

instance HasAbilities RandolphCarterExpertDreamer where
  getAbilities (RandolphCarterExpertDreamer x) =
    [ controlledAbility x 1 (exists $ You <> can.draw.cards FromPlayerCardEffect)
        $ ReactionAbility (RevealChaosToken #when (InvestigatorAt YourLocation) $ ChaosTokenFaceIs Tablet)
        $ exhaust x
    ]

instance RunMessage RandolphCarterExpertDreamer where
  runMessage msg a@(RandolphCarterExpertDreamer attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (Window.revealedChaosTokens -> tokens) _ -> do
      drawing <- drawCards iid (toAbilitySource attrs 1) 2
      push $ If (Window.RevealChaosTokenAssetAbilityEffect iid tokens (toId attrs)) [drawing]
      pure a
    _ -> RandolphCarterExpertDreamer <$> runMessage msg attrs
