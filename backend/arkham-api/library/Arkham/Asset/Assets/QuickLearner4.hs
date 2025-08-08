module Arkham.Asset.Assets.QuickLearner4 (quickLearner4) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Control.Monad.Fail (fail)

newtype QuickLearner4 = QuickLearner4 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quickLearner4 :: AssetCard QuickLearner4
quickLearner4 = asset QuickLearner4 Cards.quickLearner4

instance HasModifiersFor QuickLearner4 where
  getModifiersFor (QuickLearner4 a) =
    getSkillTest >>= traverse_ \st -> do
      maybeModified_ a (SkillTestTarget st.id) do
        guard $ controlledBy a st.investigator
        guardMatches st.investigator TurnInvestigator
        inAction <- getGameInAction
        actionsTaken <- lift $ fieldMap InvestigatorActionsTaken length st.investigator
        case actionsTaken + if inAction then 1 else 0 of
          n | n < 2 -> pure [Difficulty 1]
          n | n > 2 -> pure [Difficulty (-1)]
          _ -> fail "Wrong number of actions taken"

instance RunMessage QuickLearner4 where
  runMessage msg (QuickLearner4 attrs) = QuickLearner4 <$> runMessage msg attrs
