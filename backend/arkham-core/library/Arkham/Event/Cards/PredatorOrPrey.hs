module Arkham.Event.Cards.PredatorOrPrey (predatorOrPrey, PredatorOrPrey (..)) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Movement

newtype PredatorOrPrey = PredatorOrPrey EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

predatorOrPrey :: EventCard PredatorOrPrey
predatorOrPrey = event PredatorOrPrey Cards.predatorOrPrey

instance RunMessage PredatorOrPrey where
  runMessage msg e@(PredatorOrPrey attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <- select AnyEnemy
      if null enemies
        then drawCardsIfCan iid attrs 1
        else do
          unengagedEnemies <-
            mapMaybe (\(a, mb) -> (a,) <$> mb) <$> selectWithField EnemyLocation UnengagedEnemy
          investigators <- select Anyone
          chooseOneM iid do
            when (notNull unengagedEnemies) do
              labeled "Each unengaged enemy moves once toward the nearest investigator." do
                chooseOneAtATime
                  iid
                  [ targetLabel
                    enemy
                    [ Move
                        $ moveTowardsMatching attrs enemy
                        $ NearestLocationToLocation loc
                        $ LocationWithInvestigator Anyone
                    ]
                  | (enemy, loc) <- unengagedEnemies
                  ]

            labeled
              "Each investigator disengages from each enemy engaged with them and moves once away from the nearest enemy."
              $ handleOneAtATime iid attrs investigators

      pure e
    HandleTargetChoice _ (isSource attrs -> True) (InvestigatorTarget iid) -> do
      engagedEnemies <- select $ enemyEngagedWith iid
      for_ engagedEnemies $ disengageEnemy iid
      withLocationOf iid \loc -> do
        locations <-
          select
            $ CanMoveToLocation (InvestigatorWithId iid) (toSource attrs)
            $ AccessibleFrom (LocationWithId loc)
            <> LocationFartherFrom loc (NearestLocationTo iid $ LocationWithEnemy AnyEnemy)
        when (notNull locations) do
          chooseOne iid [targetLabel loc' [Move $ move attrs iid loc'] | loc' <- locations]
      pure e
    _ -> PredatorOrPrey <$> liftRunMessage msg attrs
