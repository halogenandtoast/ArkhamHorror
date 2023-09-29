module Arkham.Asset.Cards.RecallTheFuture2 (
  recallTheFuture2,
  RecallTheFuture2 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosBag.Base
import Arkham.ChaosToken
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Scenario.Types (Field (..))
import Arkham.Timing qualified as Timing
import Arkham.Window qualified as Window

newtype Metadata = Metadata {chosenChaosToken :: Maybe ChaosTokenFace}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype RecallTheFuture2 = RecallTheFuture2 (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

recallTheFuture2 :: AssetCard RecallTheFuture2
recallTheFuture2 = asset (RecallTheFuture2 . (`with` Metadata Nothing)) Cards.recallTheFuture2

instance HasAbilities RecallTheFuture2 where
  getAbilities (RecallTheFuture2 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (InitiatedSkillTest Timing.When You AnySkillType AnySkillTestValue)
          Free
    ]

instance RunMessage RecallTheFuture2 where
  runMessage msg a@(RecallTheFuture2 (attrs `With` metadata)) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      tokens <-
        scenarioFieldMap
          ScenarioChaosBag
          (nub . map chaosTokenFace . allChaosBagChaosTokens)
      push
        $ chooseOne
          iid
          [ Label
            (tshow t)
            [HandleTargetChoice iid (toSource attrs) (ChaosTokenFaceTarget t)]
          | t <- tokens
          ]
      pure a
    HandleTargetChoice _ (isSource attrs -> True) (ChaosTokenFaceTarget t) -> do
      pure . RecallTheFuture2 $ attrs `with` Metadata (Just t)
    When (Msg.RevealChaosToken _ _ token@(ChaosToken _ t)) | Just t == chosenChaosToken metadata -> do
      unless (assetExhausted attrs) $ for_ (assetController attrs) $ \iid -> do
        push
          $ If
            (Window.RevealChaosTokenAssetAbilityEffect iid [token] (toId attrs))
            [ ExhaustThen
                (toTarget attrs)
                [ skillTestModifier
                    attrs
                    (InvestigatorTarget iid)
                    (AnySkillValue 2)
                ]
            ]
      pure a
    SkillTestEnds _ _ ->
      pure . RecallTheFuture2 $ attrs `with` Metadata Nothing
    _ -> RecallTheFuture2 . (`with` metadata) <$> runMessage msg attrs
