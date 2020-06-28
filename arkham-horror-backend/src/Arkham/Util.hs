module Arkham.Util where

import Arkham.Conversion
import Arkham.Internal.Types
import Arkham.Types
import ClassyPrelude
import Database.Persist.Sql
import Lens.Micro

drawCard :: ArkhamGameData -> ArkhamGameData
drawCard g =
  let (drawn, deck') = splitAt 1 (g ^. player . deck)
  in g & player . hand %~ (++ drawn) & player . deck .~ deck'

updateGame
  :: (MonadIO m) => ArkhamGameId -> ArkhamGame -> SqlPersistT m ArkhamGameData
updateGame gameId game = replace gameId updatedGame
  $> arkhamGameCurrentData updatedGame
  where updatedGame = runGamePhase game

-- brittany-disable-next-binding
runGamePhase :: ArkhamGame -> ArkhamGame
runGamePhase g = removeLock $ until isLocked go $ go (buildLock g)
  where
    scenario' = toInternalScenario g
    ArkhamInvestigationPhaseInternal {..} = scenarioInvestigationPhase scenario'
    ArkhamMythosPhaseInternal {..} = scenarioMythosPhase scenario'
    go g' = g'
      & mythosAddDoom
      & mythosCheckAdvance
      & mythosDrawEncounter
      & mythosOnEnd
      & investigationPhaseOnEnter
      & investigationPhaseTakeActions
      & investigationPhaseOnExit
      & runLocked "enemyPhase" (\g'' -> Unlocked $ g'' & phase .~ Upkeep)
      & runLocked "upkeepPhase" (\g'' -> Unlocked $ g'' & currentData %~ drawCard & player . resources +~ 1 & phase .~ Mythos)
