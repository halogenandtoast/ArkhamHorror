module Arkham.Helpers.History where

import Arkham.Classes.HasGame
import Arkham.Helpers.GameValue (gameValueMatches)
import Arkham.History
import Arkham.Matcher qualified as Matcher
import Arkham.Prelude

historyMatches :: HasGame m => Matcher.HistoryMatcher -> History -> m Bool
historyMatches = \case
  Matcher.DefeatedEnemiesWithTotalHealth vMatcher ->
    (`gameValueMatches` vMatcher) . sum . map defeatedEnemyHealth . historyEnemiesDefeated
