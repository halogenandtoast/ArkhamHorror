module Arkham.Helpers.Defeat where

import Arkham.Classes.HasGame
import Arkham.DefeatedBy
import Arkham.Helpers.Source
import Arkham.Matcher qualified as Matcher
import Arkham.Prelude

defeatedByMatches
  :: HasGame m => DefeatedBy -> Matcher.DefeatedByMatcher -> m Bool
defeatedByMatches defeatedBy = \case
  Matcher.ByAnyOf xs -> anyM (defeatedByMatches defeatedBy) xs
  Matcher.ByHorror -> pure $ wasDefeatedByHorror defeatedBy
  Matcher.ByDamage -> pure $ wasDefeatedByDamage defeatedBy
  Matcher.ByOther -> pure $ wasDefeatedByOther defeatedBy
  Matcher.BySource sourceMatcher -> sourceMatches (defeatedBySource defeatedBy) sourceMatcher
  Matcher.ByAny -> pure True
  Matcher.DefeatedByMatches xs -> allM (defeatedByMatches defeatedBy) xs
  Matcher.NotBy x -> not <$> defeatedByMatches defeatedBy x
