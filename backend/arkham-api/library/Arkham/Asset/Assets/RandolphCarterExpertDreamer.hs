module Arkham.Asset.Assets.RandolphCarterExpertDreamer (
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
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

randolphCarterExpertDreamer :: AssetCard RandolphCarterExpertDreamer
randolphCarterExpertDreamer = ally RandolphCarterExpertDreamer Cards.randolphCarterExpertDreamer (3, 2)

instance HasModifiersFor RandolphCarterExpertDreamer where
  getModifiersFor (RandolphCarterExpertDreamer a) = case a.controller of
    Just iid -> modified_ a iid [SkillModifier #combat 1, SkillModifier #agility 1]
    Nothing -> pure mempty

instance HasAbilities RandolphCarterExpertDreamer where
  getAbilities (RandolphCarterExpertDreamer x) =
    [ controlledAbility x 1 (exists $ You <> can.draw.cards FromPlayerCardEffect)
        $ ReactionAbility (RevealChaosToken #when (InvestigatorAt YourLocation) $ ChaosTokenFaceIs Tablet)
        $ exhaust x
    ]

instance RunMessage RandolphCarterExpertDreamer where
  runMessage msg a@(RandolphCarterExpertDreamer attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (Window.revealedChaosTokens -> tokens) _ -> do
      let drawing = drawCards iid (toAbilitySource attrs 1) 2
      push $ If (Window.RevealChaosTokenAssetAbilityEffect iid tokens (toId attrs)) [drawing]
      pure a
    _ -> RandolphCarterExpertDreamer <$> runMessage msg attrs
