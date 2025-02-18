module Arkham.Helpers.Phase where

import Arkham.Matcher qualified as Matcher
import Arkham.Phase
import Arkham.Prelude

matchPhase :: Monad m => Phase -> Matcher.PhaseMatcher -> m Bool
matchPhase p = \case
  Matcher.AnyPhase -> pure True
  Matcher.IsMythosPhase -> case p of
    MythosPhase {} -> pure True
    _ -> pure False
  Matcher.IsEnemyPhase -> case p of
    EnemyPhase {} -> pure True
    _ -> pure False
  Matcher.IsInvestigationPhase -> case p of
    InvestigationPhase {} -> pure True
    _ -> pure False
  Matcher.IsUpkeepPhase -> case p of
    UpkeepPhase {} -> pure True
    _ -> pure False
