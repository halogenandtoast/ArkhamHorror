module Arkham.Story.Cards.BarriersDecoysAndTraps (barriersDecoysAndTraps) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectMaybe)
import Arkham.Helpers.Movement (cancelEnemyMovement)
import Arkham.Helpers.Scenario (scenarioField)
import Arkham.Helpers.Window.Enemy (enteringEnemy)
import Arkham.Id
import Arkham.Location.Types (Field (LocationPosition))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheLongestNight.Helpers
import Arkham.SortedPair
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Token
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype BarriersDecoysAndTraps = BarriersDecoysAndTraps StoryAttrs
  deriving anyclass IsStory
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor BarriersDecoysAndTraps where
  getModifiersFor (BarriersDecoysAndTraps a) = do
    scenarioMeta <- scenarioField ScenarioMeta
    let Meta meta _ = toResultDefault (Meta mempty False) scenarioMeta
    modifySelectMaybe a Anywhere \lid -> do
      pos <- MaybeT $ field LocationPosition lid
      let
        barrierMods (pair, n) = case unSortedPair pair of
          (l1, l2) | l1 == lid && n > 0 -> do
            pos2 <- lift $ field LocationPosition l2
            pure $ case pos2 >>= directionBetween pos of
              Just dir -> [Barricades [l2], ScenarioModifier (barrierModifier dir)]
              Nothing -> [Barricades [l2]]
          (l1, l2) | l2 == lid && n > 0 -> do
            pos1 <- lift $ field LocationPosition l1
            pure $ case pos1 >>= directionBetween pos of
              Just dir -> [Barricades [l1], ScenarioModifier (barrierModifier dir)]
              Nothing -> [Barricades [l1]]
          _ -> pure []
      mods <- concat <$> traverse barrierMods (mapToList meta)
      guard $ notNull mods
      pure mods

barriersDecoysAndTraps :: StoryCard BarriersDecoysAndTraps
barriersDecoysAndTraps = story BarriersDecoysAndTraps Cards.barriersDecoysAndTraps

instance HasAbilities BarriersDecoysAndTraps where
  getAbilities (BarriersDecoysAndTraps a) =
    [ mkAbility a 1
        $ forced
        $ OrWindowMatcher
          [ EnemyWouldMove #when enemyMatcher AnySource (loc "barrierNorth") (loc "barrierSouth")
          , EnemyWouldMove #when enemyMatcher AnySource (loc "barrierSouth") (loc "barrierNorth")
          , EnemyWouldMove #when enemyMatcher AnySource (loc "barrierEast") (loc "barrierWest")
          , EnemyWouldMove #when enemyMatcher AnySource (loc "barrierWest") (loc "barrierEast")
          ]
    , mkAbility a 2
        $ forced
        $ EnemyEnters #when (LocationWithHorror (atLeast 1)) (EnemyWithoutModifier IgnoreDecoys)
    , mkAbility a 3
        $ forced
        $ EnemyEnters #when (LocationWithDamage (atLeast 1)) (EnemyWithoutModifier IgnoreTraps)
    ]
   where
    enemyMatcher = EnemyWithoutModifier IgnoreBarriers
    loc m = LocationWithModifier (ScenarioModifier m)

instance RunMessage BarriersDecoysAndTraps where
  runMessage msg s@(BarriersDecoysAndTraps attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    UseCardAbility _ (isSource attrs -> True) 1 ws _ -> do
      case mapMaybe getWouldMoveDetails ws of
        (eid, fromLid, toLid) : _ -> do
          scenarioMeta <- scenarioField ScenarioMeta
          let Meta meta _ = toResultDefault (Meta mempty False) scenarioMeta
          let n = findWithDefault 0 (sortedPair fromLid toLid) meta
          when (n > 0) do
            push $ ScenarioCountDecrementBy (Barriers fromLid toLid) 1
            cancelEnemyMovement eid
        _ -> pure ()
      pure s
    UseCardAbility _ (isSource attrs -> True) 2 ws _ -> do
      let eid = enteringEnemy ws
      let lid = getEnteringLocation ws
      removeTokens attrs lid Horror 1
      nonAttackEnemyDamage Nothing (toSource attrs) 1 eid
      disengageFromAll eid
      exhaustEnemy (toSource attrs) eid
      roundModifier attrs eid CannotReady
      pure s
    UseCardAbility _ (isSource attrs -> True) 3 ws _ -> do
      let eid = enteringEnemy ws
      let lid = getEnteringLocation ws
      removeTokens attrs lid Damage 1
      nonAttackEnemyDamage Nothing (toSource attrs) 2 eid
      pure s
    _ -> BarriersDecoysAndTraps <$> liftRunMessage msg attrs

getWouldMoveDetails :: Window -> Maybe (EnemyId, LocationId, LocationId)
getWouldMoveDetails (windowType -> Window.EnemyWouldMove eid _ fromLid toLid) = Just (eid, fromLid, toLid)
getWouldMoveDetails _ = Nothing

getEnteringLocation :: HasCallStack => [Window] -> LocationId
getEnteringLocation = go
 where
  go [] = error "invalid window"
  go ((windowType -> Window.EnemyEnters _ lid) : _) = lid
  go (_ : rest) = go rest
