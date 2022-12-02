module Arkham.Asset.Cards.RecallTheFuture2
  ( recallTheFuture2
  , RecallTheFuture2(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosBag.Base
import Arkham.Cost
import Arkham.Criteria
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Matcher
import Arkham.Scenario.Types ( Field (..) )
import Arkham.SkillTest
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Token

newtype Metadata = Metadata { chosenToken :: Maybe TokenFace }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype RecallTheFuture2 = RecallTheFuture2 (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

recallTheFuture2 :: AssetCard RecallTheFuture2
recallTheFuture2 =
  asset (RecallTheFuture2 . (`with` Metadata Nothing)) Cards.recallTheFuture2

instance HasAbilities RecallTheFuture2 where
  getAbilities (RecallTheFuture2 a) =
    [ restrictedAbility a 1 ControlsThis $ ReactionAbility
        (InitiatedSkillTest Timing.When You AnySkillType AnySkillTestValue)
        Free
    ]

instance RunMessage RecallTheFuture2 where
  runMessage msg a@(RecallTheFuture2 (attrs `With` metadata)) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      tokens <-
        nub
        . map tokenFace
        . allChaosBagTokens
        <$> scenarioField ScenarioChaosBag
      push $ chooseOne
        iid
        [ Label
            (tshow t)
            [HandleTargetChoice iid (toSource attrs) (TokenFaceTarget t)]
        | t <- tokens
        ]
      pure a
    HandleTargetChoice _ (isSource attrs -> True) (TokenFaceTarget t) -> do
      pure . RecallTheFuture2 $ attrs `with` Metadata (Just t)
    When (RevealToken _ _ (Token _ t)) | Just t == chosenToken metadata -> do
      unless (assetExhausted attrs) $
        for_ (assetController attrs) $ \iid -> do
          skillType <- skillTestSkillType <$> getJustSkillTest
          pushAll
            [ Exhaust (toTarget attrs)
            , skillTestModifier attrs (InvestigatorTarget iid)
              $ SkillModifier skillType 2
            ]
      pure a
    SkillTestEnds _ _ ->
      pure . RecallTheFuture2 $ attrs `with` Metadata Nothing
    _ -> RecallTheFuture2 . (`with` metadata) <$> runMessage msg attrs
