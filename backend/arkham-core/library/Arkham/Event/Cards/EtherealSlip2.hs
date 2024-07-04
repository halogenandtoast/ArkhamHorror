module Arkham.Event.Cards.EtherealSlip2 (etherealSlip2, EtherealSlip2 (..)) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator (getJustLocation)
import Arkham.Matcher

newtype EtherealSlip2 = EtherealSlip2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

etherealSlip2 :: EventCard EtherealSlip2
etherealSlip2 = event EtherealSlip2 Cards.etherealSlip2

instance RunMessage EtherealSlip2 where
  runMessage msg e@(EtherealSlip2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      location <- getJustLocation iid
      enemies <-
        selectWithField EnemyLocation
          $ NonEliteEnemy
          <> EnemyAt
            (RevealedLocation <> CanEnterLocation (InvestigatorWithId iid))
          <> EnemyCanEnter (LocationWithId location)
      chooseOrRunOne
        iid
        [ targetLabel enemy [SwapPlaces (toTarget enemy, enemyLocation) (toTarget iid, location)]
        | (enemy, Just enemyLocation) <- enemies
        ]

      pure e
    _ -> EtherealSlip2 <$> liftRunMessage msg attrs
