module Arkham.Helpers.ChaosBag where

import Arkham.Prelude

import Arkham.Asset.Types (Field (..))
import Arkham.ChaosBag.Base
import Arkham.ChaosBagStepState
import Arkham.ChaosToken.Types
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Types (Field (..))
import Arkham.Homebrew.Tokens (chaosTokenFacePool)
import Arkham.Helpers.Scenario
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Scenario.Types (Field (..))

-- These can be queried outside of a scenario (e.g. while gathering actions
-- during the between-scenarios deck-upgrade step, where the scenario has
-- already been torn out of the game mode). With no scenario there is no chaos
-- bag, so degrade to "no tokens" rather than crashing on the missing field.
getOnlyChaosTokensInBag :: HasGame m => m [ChaosToken]
getOnlyChaosTokensInBag = foldMap chaosBagChaosTokens <$> scenarioFieldMaybe ScenarioChaosBag

getBagChaosTokens :: (HasCallStack, HasGame m) => m [ChaosToken]
getBagChaosTokens = foldMap allChaosBagChaosTokens <$> scenarioFieldMaybe ScenarioChaosBag

getTokenPool :: HasGame m => m [ChaosToken]
getTokenPool = foldMap chaosBagTokenPool <$> scenarioFieldMaybe ScenarioChaosBag

getRemainingFrostTokens :: HasGame m => m Int
getRemainingFrostTokens = selectCount $ InTokenPool #frost

hasRemainingFrostTokens :: HasGame m => m Bool
hasRemainingFrostTokens = (> 0) <$> getRemainingFrostTokens

getRemainingCurseTokens :: HasGame m => m Int
getRemainingCurseTokens = selectCount $ InTokenPool #curse

getRemainingBlessTokens :: HasGame m => m Int
getRemainingBlessTokens = selectCount $ InTokenPool #bless

getRemainingBloodTokens :: HasGame m => m Int
getRemainingBloodTokens = selectCount $ InTokenPool #blood

-- | Whether n more tokens of this face could be added to the chaos bag.
canAddChaosTokenFaces :: HasGame m => Int -> ChaosTokenFace -> m Bool
canAddChaosTokenFaces n face = case chaosTokenFacePool face of
  Nothing -> pure True
  Just _ -> (>= n) <$> selectCount (InTokenPool $ ChaosTokenFaceIs face)

canAddChaosTokenFace :: HasGame m => ChaosTokenFace -> m Bool
canAddChaosTokenFace = canAddChaosTokenFaces 1

getSealedChaosTokens :: HasGame m => m [ChaosToken]
getSealedChaosTokens =
  concat
    <$> sequence
      [ selectAgg id AssetSealedChaosTokens AnyAsset
      , selectAgg id EnemySealedChaosTokens AnyEnemy
      , selectAgg id EventSealedChaosTokens AnyEvent
      , selectAgg id InvestigatorSealedChaosTokens Anyone
      ]

getAllChaosTokens :: HasGame m => m [ChaosToken]
getAllChaosTokens = nub . concat <$> sequence [getBagChaosTokens, getSealedChaosTokens]

getChaosBagChoice :: HasGame m => m (Maybe ChaosBagStepState)
getChaosBagChoice = scenarioFieldMap ScenarioChaosBag chaosBagChoice

getChaosBag :: HasGame m => m ChaosBag
getChaosBag = scenarioField ScenarioChaosBag

-- | Extract the chain of step-states wrapped in a chaos-bag choice.
--
-- A 'Decided' state means the choice's composition has been committed but its
-- draws have not yet been resolved into tokens — at that point the inner
-- steps are still meaningful to a second reactor (e.g. Jacqueline Fine
-- composing with an already-resolved Eyes of the Dreamer setup). We
-- therefore extract from both 'Deciding' and 'Decided'. 'Resolved' is
-- intentionally empty: by then tokens have been physically drawn and a
-- second reactor cannot retroactively add to the pool.
getSteps :: ChaosBagStepState -> [ChaosBagStepState]
getSteps = \case
  Resolved {} -> []
  Decided s -> go s
  Undecided s -> go s
  Deciding s -> go s
 where
  go = \case
    Draw -> [Undecided Draw]
    DrawUntil inner -> [Undecided (DrawUntil inner)]
    Choose {..} -> steps
    ChooseMatch {..} -> steps
    ChooseMatchChoice {..} -> steps
