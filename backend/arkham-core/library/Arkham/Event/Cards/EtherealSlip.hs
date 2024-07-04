module Arkham.Event.Cards.EtherealSlip (etherealSlip, EtherealSlip (..)) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator (getJustLocation)
import Arkham.Matcher

newtype EtherealSlip = EtherealSlip EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

etherealSlip :: EventCard EtherealSlip
etherealSlip = event EtherealSlip Cards.etherealSlip

instance RunMessage EtherealSlip where
  runMessage msg e@(EtherealSlip attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      location <- getJustLocation iid
      enemies <-
        selectWithField EnemyLocation
          $ NonEliteEnemy
          <> EnemyAt
            ( LocationWithDistanceFromAtMost
                2
                (LocationWithId location)
                (RevealedLocation <> CanEnterLocation (InvestigatorWithId iid))
            )
          <> EnemyCanEnter (LocationWithId location)
      chooseOrRunOne
        iid
        [ targetLabel enemy [SwapPlaces (toTarget enemy, enemyLocation) (toTarget iid, location)]
        | (enemy, Just enemyLocation) <- enemies
        ]

      pure e
    _ -> EtherealSlip <$> liftRunMessage msg attrs
