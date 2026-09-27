module Arkham.Scenarios.TheFeastOfHemlockVale.TheTwistedHollow.Helpers where

import Arkham.Asset.Types (AssetAttrs)
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Effect.Import (
  EffectAttrs,
  EffectMetadata (..),
  disable,
  getEffectMetaDefault,
  setEffectMeta,
 )
import Arkham.Enemy.Types (Enemy, Field (..))
import Arkham.GameEnv (getWindowDepth)
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Scenario (scenarioFieldMap)
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher hiding (EnemyDrawnFrom)
import Arkham.Message (Message (..), pattern InvestigatorDrawEnemy)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Message.Lifted.Queue
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Arkham.Target (Target (LocationTarget))
import Arkham.Token
import Arkham.Window qualified as Window
import Arkham.Zone

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "theTwistedHollow" a

getDarknessLevel :: HasGame m => m Int
getDarknessLevel = scenarioFieldMap ScenarioTokens (countTokens DarknessLevel)

pursuitEnemiesWithHighestFight :: HasGame m => m [EnemyId]
pursuitEnemiesWithHighestFight = do
  enemies <- select $ OutOfPlayEnemy PursuitZone EnemyWithFight
  fightValue <-
    selectAgg' @(OutOfPlayEntity 'PursuitZone Enemy)
      (Max0 . fromMaybe 0)
      (OutOfPlayEnemyField PursuitZone EnemyFight)
      (OutOfPlayEnemy PursuitZone EnemyWithFight)
  filterM
    ( fieldMap @(OutOfPlayEntity 'PursuitZone Enemy)
        (OutOfPlayEnemyField PursuitZone EnemyFight)
        (== Just fightValue)
    )
    enemies

pursuitEnemiesWithLowestFight :: HasGame m => m [EnemyId]
pursuitEnemiesWithLowestFight = do
  enemies <- select $ OutOfPlayEnemy PursuitZone EnemyWithFight
  fightValue <-
    selectAgg' @(OutOfPlayEntity 'PursuitZone Enemy)
      (Min . fromMaybe 0)
      (OutOfPlayEnemyField PursuitZone EnemyFight)
      (OutOfPlayEnemy PursuitZone EnemyWithFight)
  filterM
    ( fieldMap @(OutOfPlayEntity 'PursuitZone Enemy)
        (OutOfPlayEnemyField PursuitZone EnemyFight)
        (== Just fightValue)
    )
    enemies

pursuitEnemiesWithHighestHealth :: HasGame m => m [EnemyId]
pursuitEnemiesWithHighestHealth = do
  enemies <- select $ OutOfPlayEnemy PursuitZone EnemyWithHealth
  healthValue <-
    selectAgg' @(OutOfPlayEntity 'PursuitZone Enemy)
      (Max0 . fromMaybe 0)
      (OutOfPlayEnemyField PursuitZone EnemyHealth)
      (OutOfPlayEnemy PursuitZone EnemyWithHealth)
  filterM
    ( fieldMap @(OutOfPlayEntity 'PursuitZone Enemy)
        (OutOfPlayEnemyField PursuitZone EnemyHealth)
        (== Just healthValue)
    )
    enemies

pursuitEnemiesWithHighestEvade :: HasGame m => m [EnemyId]
pursuitEnemiesWithHighestEvade = do
  enemies <- select $ OutOfPlayEnemy PursuitZone EnemyWithEvade
  evadeValue <-
    selectAgg' @(OutOfPlayEntity 'PursuitZone Enemy)
      (Max0 . fromMaybe 0)
      (OutOfPlayEnemyField PursuitZone EnemyEvade)
      (OutOfPlayEnemy PursuitZone EnemyWithEvade)
  filterM
    ( fieldMap @(OutOfPlayEntity 'PursuitZone Enemy)
        (OutOfPlayEnemyField PursuitZone EnemyEvade)
        (== Just evadeValue)
    )
    enemies

pursuitEnemiesWithLowestEvade :: HasGame m => m [EnemyId]
pursuitEnemiesWithLowestEvade = do
  enemies <- select $ OutOfPlayEnemy PursuitZone EnemyWithEvade
  evadeValue <-
    selectAgg' @(OutOfPlayEntity 'PursuitZone Enemy)
      (Min . fromMaybe 0)
      (OutOfPlayEnemyField PursuitZone EnemyEvade)
      (OutOfPlayEnemy PursuitZone EnemyWithEvade)
  filterM
    ( fieldMap @(OutOfPlayEntity 'PursuitZone Enemy)
        (OutOfPlayEnemyField PursuitZone EnemyEvade)
        (== Just evadeValue)
    )
    enemies

{- | Vale Lantern's "when it would leave play, place it at the nearest location, instead".

The lantern's own location is normally the nearest one, and the ability is offered to
every player, so fall back to the triggering investigator when the lantern's controller
has already been unplaced (resigned). If the lantern is leaving play *because* its
location is leaving play, that location is not a legal destination — placing it there
anyway orphaned the lantern on a deleted location (#5267) — so walk out to the nearest
location that is sticking around.
-}
placeValeLanternAtNearestLocation :: ReverseQueue m => InvestigatorId -> AssetAttrs -> m ()
placeValeLanternAtNearestLocation iid attrs = do
  manchor <- getLocationOf attrs >>= maybe (getLocationOf iid) (pure . Just)
  for_ manchor \anchor -> do
    stillOnBoard <- anchor <=~> not_ LocationBeingRemoved
    if stillOnBoard
      then place attrs (AtLocation anchor)
      else do
        nearest <- select $ NearestLocationToLocation anchor (not_ LocationBeingRemoved)
        chooseOrRunOneM iid $ targets nearest $ place attrs . AtLocation

drawEnemyFromPursuit :: ReverseQueue m => InvestigatorId -> EnemyId -> m ()
drawEnemyFromPursuit iid eid = do
  push $ UpdateEnemy eid $ Update EnemyDrawnFrom Nothing
  push $ InvestigatorDrawEnemy iid eid

{- | Vale Lantern's "they ignore that location's forced effect(s)" suppression, shared by
both lit sides. The suppression has to last the whole @RevealLocation #after@ window, and
that window is processed more than once: @runWindow@ queues another @Do (CheckWindows ws)@
behind every ask, so a window carrying two forced abilities (Deepening Dark and the
location itself) comes back around. Disabling on the first pass let the location's own
Forced ability through on a later one (#5782), so arm on the way in -- recording the window
depth, as Fox Mask does -- and disable once that window closes. 'Nothing' means "not mine",
so the caller falls through to the generic effect runner.
-}
valeLanternIgnoresForcedEffect
  :: ReverseQueue m => Message -> EffectAttrs -> m (Maybe EffectAttrs)
valeLanternIgnoresForcedEffect msg attrs = case msg of
  Do (CheckWindows ws) | any isRevealAfterWindow ws -> do
    depth <- getWindowDepth
    pure $ Just $ setEffectMeta (Just depth) attrs
  EndCheckWindow -> do
    depth <- getWindowDepth
    case getEffectMetaDefault @(Maybe Int) Nothing attrs of
      Just d | depth < d -> disable attrs >> pure (Just attrs)
      _ -> pure Nothing
  _ -> pure Nothing
 where
  destLid = case attrs.metadata of
    Just (EffectMetaTarget (LocationTarget lid)) -> Just lid
    _ -> Nothing
  isRevealAfterWindow w = case w.kind of
    Window.RevealLocation _ loc -> w.timing == #after && Just loc == destLid
    Window.RevealLocationByGroup loc -> w.timing == #after && Just loc == destLid
    _ -> False
