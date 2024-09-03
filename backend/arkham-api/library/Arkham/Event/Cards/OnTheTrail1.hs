module Arkham.Event.Cards.OnTheTrail1 (onTheTrail1, OnTheTrail1 (..)) where

import Arkham.Capability
import Arkham.Discover
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Message (handleTargetChoice)
import Arkham.Helpers.Query (getPlayer)
import Arkham.Matcher hiding (DiscoverClues)
import Arkham.Message qualified as Msg
import Arkham.Movement
import Arkham.Projection

newtype OnTheTrail1 = OnTheTrail1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheTrail1 :: EventCard OnTheTrail1
onTheTrail1 = event OnTheTrail1 Cards.onTheTrail1

instance RunMessage OnTheTrail1 where
  runMessage msg e@(OnTheTrail1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      enemies <- select $ EnemyAt $ not_ $ locationWithInvestigator iid
      chooseOne iid [targetLabel enemy [handleTargetChoice iid attrs enemy] | enemy <- enemies]
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget eid) -> do
      field EnemyLocation eid >>= traverse_ \lid -> do
        canMoveTowards <-
          selectAny $ CanMoveCloserToLocation (toSource attrs) (can.move iid) (LocationWithId lid)
        canDiscoverClues <-
          select
            $ LocationBetween
              (locationWithInvestigator iid)
              (LocationWithId lid)
              (EmptyLocation <> LocationWithDiscoverableCluesBy (InvestigatorWithId iid))

        let doMove = (move attrs iid lid) {moveMeans = Towards}
        player <- getPlayer iid
        chooseOrRunOne
          iid
          $ [ Label "Move twice towards the enemy" [Move $ doMove {moveAfter = [Move doMove]}] | canMoveTowards
            ]
          <> [ Label
              "Discover 1 clue at any empty location between you and the chosen enemy"
              [ Msg.chooseOne
                  player
                  [ targetLabel location [DiscoverClues iid $ discover location attrs 1] | location <- canDiscoverClues
                  ]
              ]
             | notNull canDiscoverClues
             ]

      pure e
    _ -> OnTheTrail1 <$> liftRunMessage msg attrs
