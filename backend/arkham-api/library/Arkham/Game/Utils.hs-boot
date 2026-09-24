module Arkham.Game.Utils where

import Arkham.Classes.HasGame
import Arkham.Game.Base (Game, GameMode)
import Arkham.Id
import Arkham.Investigator.Types (InvestigatorAttrs)
import Arkham.Prelude
import Arkham.Scenario.Types (Scenario)
import Arkham.Source (Source)

maybeEnemyLocation :: HasGame m => LocationId -> m (Maybe EnemyLocationId)
setScenario :: Scenario -> GameMode -> GameMode
sourceCanClaimUseAbility :: HasGame m => Source -> m Bool
withInvestigatorEdit
  :: HasGame m => InvestigatorId -> (InvestigatorAttrs -> InvestigatorAttrs) -> ReaderT Game m a -> m a
