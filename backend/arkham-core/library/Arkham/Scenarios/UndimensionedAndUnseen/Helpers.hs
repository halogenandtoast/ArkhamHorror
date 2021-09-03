module Arkham.Scenarios.UndimensionedAndUnseen.Helpers where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher

getBroodOfYogSothoth
  :: (MonadReader env m, Query EnemyMatcher env) => m [EnemyId]
getBroodOfYogSothoth = selectList $ EnemyWithTitle "Brood of Yog-Sothoth"
