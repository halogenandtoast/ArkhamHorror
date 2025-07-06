module Arkham.Event.Events.OnTheTrail1 (onTheTrail1) where

import Arkham.Capability
import Arkham.Discover
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Message (handleTargetChoice)
import Arkham.Matcher hiding (DiscoverClues)
import Arkham.Message.Lifted.Move
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

        chooseOrRunOneM iid do
          when canMoveTowards do
            labeled "Move twice towards the enemy" do
              moveToEdit attrs iid lid \m -> m {moveMeans = TowardsN 2}

          when (notNull canDiscoverClues) do
            labeled "Discover 1 clue at any empty location between you and the chosen enemy" do
              chooseTargetM iid canDiscoverClues $ discoverAt NotInvestigate iid attrs 1
      pure e
    _ -> OnTheTrail1 <$> liftRunMessage msg attrs
