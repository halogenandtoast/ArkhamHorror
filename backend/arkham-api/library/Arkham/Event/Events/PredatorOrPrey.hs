module Arkham.Event.Events.PredatorOrPrey (predatorOrPrey) where

import Arkham.Classes.HasGame
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.ForMovement
import Arkham.Helpers.Location (getLocationOf, withLocationOf)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Tracing

newtype PredatorOrPrey = PredatorOrPrey EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

predatorOrPrey :: EventCard PredatorOrPrey
predatorOrPrey = event PredatorOrPrey Cards.predatorOrPrey

-- | The tied-nearest enemies that have at least one eligible "move away" destination,
-- each paired with those destinations. When several enemies tie for nearest, the player
-- chooses which one to flee from, then moves away from that specific enemy (issue #4643).
fleeOptions
  :: (HasGame m, Tracing m)
  => EventAttrs -> InvestigatorId -> LocationId -> m [(EnemyId, [LocationId])]
fleeOptions attrs iid loc = do
  nearest <- selectWithField EnemyLocation (NearestEnemyTo iid AnyEnemy)
  forMaybeM nearest \(enemy, mEnemyLoc) -> case mEnemyLoc of
    Nothing -> pure Nothing
    Just enemyLoc -> do
      dests <-
        select
          $ CanMoveToLocation (InvestigatorWithId iid) (toSource attrs)
          $ AccessibleFrom ForMovement (LocationWithId loc)
          <> LocationFartherFrom loc (LocationWithId enemyLoc)
          <> not_ (LocationWithId loc)
      pure $ if null dests then Nothing else Just (enemy, dests)

instance RunMessage PredatorOrPrey where
  runMessage msg e@(PredatorOrPrey attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <- select $ AnyEnemy
      if null enemies
        then drawCardsIfCan iid attrs 1
        else do
          unengagedEnemies <-
            mapMaybe (\(a, mb) -> (a,) <$> mb) <$> selectWithField EnemyLocation (UnengagedEnemy <> NonEliteEnemy <> not_ (at_ $ locationWithInvestigator iid))
          investigators <- select Anyone
          eligibleInvestigators <- flip filterM investigators \i -> do
            engaged <- selectAny (enemyEngagedWith i)
            if engaged
              then pure True
              else
                getLocationOf i >>= \case
                  Nothing -> pure False
                  Just loc -> notNull <$> fleeOptions attrs i loc
          chooseOneM iid $ cardI18n $ scope "predatorOrPrey" do
            when (notNull unengagedEnemies) do
              labeled' "enemiesMove" do
                chooseOneAtATimeM iid do
                  for_ unengagedEnemies \(enemy, loc) ->
                    targeting enemy do
                      moveTowardsMatching attrs enemy $ NearestLocationToLocation loc $ LocationWithInvestigator Anyone
            when (notNull eligibleInvestigators) do
              labeled' "investigatorsMove"
                $ handleOneAtATime iid attrs eligibleInvestigators

      pure e
    HandleTargetChoice _ (isSource attrs -> True) (InvestigatorTarget iid) -> do
      enemies <- select (enemyEngagedWith iid)
      for_ enemies (disengageEnemy iid)
      withLocationOf iid \loc -> do
        options <- fleeOptions attrs iid loc
        chooseOrRunOneM iid do
          for_ options \(enemy, dests) ->
            targeting enemy $ chooseTargetM iid dests $ moveTo attrs iid
      -- If the investigator could not actually move away (no eligible
      -- destination), re-check engagement so the enemy re-engages instead of
      -- being left stranded and unengaged at the investigator's location.
      for_ enemies enemyCheckEngagement
      pure e
    _ -> PredatorOrPrey <$> liftRunMessage msg attrs
