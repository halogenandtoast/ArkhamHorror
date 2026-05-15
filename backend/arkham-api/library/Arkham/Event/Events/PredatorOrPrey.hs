module Arkham.Event.Events.PredatorOrPrey (predatorOrPrey) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.ForMovement
import Arkham.Helpers.Location (withLocationOf)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Move

newtype PredatorOrPrey = PredatorOrPrey EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

predatorOrPrey :: EventCard PredatorOrPrey
predatorOrPrey = event PredatorOrPrey Cards.predatorOrPrey

instance RunMessage PredatorOrPrey where
  runMessage msg e@(PredatorOrPrey attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <- select $ InPlayEnemy AnyEnemy
      if null enemies
        then drawCardsIfCan iid attrs 1
        else do
          unengagedEnemies <-
            mapMaybe (\(a, mb) -> (a,) <$> mb) <$> selectWithField EnemyLocation (UnengagedEnemy <> NonEliteEnemy <> not_ (at_ $ locationWithInvestigator iid))
          investigators <- select Anyone
          chooseOneM iid $ cardI18n $ scope "predatorOrPrey" do
            when (notNull unengagedEnemies) do
              labeled' "enemiesMove" do
                chooseOneAtATimeM iid do
                  for_ unengagedEnemies \(enemy, loc) ->
                    targeting enemy do
                      moveTowardsMatching attrs enemy $ NearestLocationToLocation loc $ LocationWithInvestigator Anyone
            labeled' "investigatorsMove"
              $ handleOneAtATime iid attrs investigators

      pure e
    HandleTargetChoice _ (isSource attrs -> True) (InvestigatorTarget iid) -> do
      selectEach (enemyEngagedWith iid) (disengageEnemy iid)
      withLocationOf iid \loc -> do
        locations <-
          select
            $ CanMoveToLocation (InvestigatorWithId iid) (toSource attrs)
            $ AccessibleFrom ForMovement (LocationWithId loc)
            <> LocationFartherFrom loc (NearestLocationTo iid $ LocationWithEnemy AnyEnemy)
            <> not_ (LocationWithId loc)
        chooseTargetM iid locations $ moveTo attrs iid
      pure e
    _ -> PredatorOrPrey <$> liftRunMessage msg attrs
