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


buildLock :: ArkhamGame -> Lockable String ArkhamGame
buildLock g = case g ^. lock of
  Just lock' -> Locked (== lock') g
  Nothing -> Unlocked g

-- brittany-disable-next-binding
runGamePhase :: ArkhamGame -> ArkhamGame
runGamePhase g = removeLock $ until isLocked go $ go (buildLock g)
  where
    scenario' = toInternalScenario g
    go g' =
      case removeLock g' ^. phase of
        Investigation ->
          let ArkhamInvestigationPhaseInternal {..} = scenarioInvestigationPhase scenario'
           in g' & investigationPhaseOnEnter & investigationPhaseTakeActions & investigationPhaseOnExit
        Enemy -> g' & runLocked "enemyPhase" (\g'' -> Unlocked $ g'' & phase .~ Upkeep)
        Upkeep -> g' & runLocked "upkeepPhase" (\g'' -> Unlocked $ g'' & currentData %~ drawCard & player . resources +~ 1 & phase .~ Mythos)
        Mythos ->
          let ArkhamMythosPhaseInternal {..} = scenarioMythosPhase scenario'
           in g' & mythosAddDoom & mythosCheckAdvance & mythosDrawEncounter & mythosOnEnd
