module Arkham.Event.Events.OnTheTrail3 (onTheTrail3) where

import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator (getCanDiscoverClues)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher hiding (DiscoverClues)
import Arkham.Message.Lifted.Move
import Arkham.Movement

newtype OnTheTrail3 = OnTheTrail3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheTrail3 :: EventCard OnTheTrail3
onTheTrail3 = event OnTheTrail3 Cards.onTheTrail3

instance RunMessage OnTheTrail3 where
  runMessage msg e@(OnTheTrail3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      enemies <- select $ EnemyAt $ not_ $ locationWithInvestigator iid
      chooseTargetM iid enemies $ handleTarget iid attrs
      doStep 2 msg
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget eid) -> do
      withLocationOf eid \lid -> do
        moveToEdit attrs iid lid \m -> m {moveMeans = TowardsN 2}
      pure e
    MoveTo movement | moveSource movement == toSource attrs -> do
      case moveDestination movement of
        ToLocation lid -> pure . OnTheTrail3 $ attrs & setMeta (lid : toResultDefault [] attrs.meta)
        _ -> pure e
    DoStep 2 (PlayThisEvent iid (is attrs -> True)) -> do
      for_ (toResultDefault [] attrs.meta) \lid -> do
        whenM (getCanDiscoverClues NotInvestigate iid lid) $ discoverAt NotInvestigate iid attrs 1 lid
      pure e
    _ -> OnTheTrail3 <$> liftRunMessage msg attrs
