module Arkham.Asset.Cards.QuickLearner4
  ( quickLearner4
  , QuickLearner4(..)
  )
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Projection
import Arkham.SkillTest.Base
import Arkham.Investigator.Types (Field(..))

newtype QuickLearner4 = QuickLearner4 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quickLearner4 :: AssetCard QuickLearner4
quickLearner4 =
  asset QuickLearner4 Cards.quickLearner4

instance HasModifiersFor QuickLearner4 where
  getModifiersFor SkillTestTarget (QuickLearner4 a) = do
    mSkillTestInvestigator <- fmap skillTestInvestigator <$> getSkillTest
    case mSkillTestInvestigator of
      Just iid | controlledBy a iid -> do
        actionsTaken <- fieldMap InvestigatorActionsTaken length iid
        case actionsTaken of
          n | n < 2 -> pure $ toModifiers a [Difficulty 1]
          n | n > 2 -> pure $ toModifiers a [Difficulty (-1)]
          _ -> pure []
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage QuickLearner4 where
  runMessage msg (QuickLearner4 attrs) = QuickLearner4 <$> runMessage msg attrs
