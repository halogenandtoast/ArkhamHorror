module Arkham.Helpers.History (module Arkham.Helpers.History, module X) where

import Arkham.Classes.HasGame
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.GameValue (gameValueMatches)
import Arkham.Helpers.Query
import Arkham.History as X
import Arkham.Id
import Arkham.Matcher qualified as Matcher
import Arkham.Prelude

historyMatches :: HasGame m => Matcher.HistoryMatcher -> History -> m Bool
historyMatches = \case
  Matcher.DefeatedEnemiesWithTotalHealth vMatcher ->
    (`gameValueMatches` vMatcher) . sum . map defeatedEnemyHealth . historyEnemiesDefeated
  Matcher.AttackedByAnyEnemies -> pure . notNull . historyEnemiesAttackedBy

getAllHistoryField :: (HasGame m, Monoid k) => HistoryType -> HistoryField k -> m k
getAllHistoryField htype fld = concatMap (viewHistoryField fld) <$> (traverse (getHistory htype) =<< getInvestigators)

hasHistory
  :: (HasGame m, AsId investigator, IdOf investigator ~ InvestigatorId)
  => HistoryType -> Matcher.HistoryMatcher -> investigator -> m Bool
hasHistory htype matcher (asId -> iid) = getHistory htype iid >>= historyMatches matcher
