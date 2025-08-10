module Arkham.Treachery.Cards.DetachedFromReality (detachedFromReality) where

import Arkham.Card
import Arkham.Helpers.Location
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (PutLocationIntoPlay)
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted
import Arkham.Window

newtype DetachedFromReality = DetachedFromReality TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

detachedFromReality :: TreacheryCard DetachedFromReality
detachedFromReality = treachery DetachedFromReality Cards.detachedFromReality

instance RunMessage DetachedFromReality where
  runMessage msg t@(DetachedFromReality attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <- select $ enemyEngagedWith iid
      selectOne (locationIs Locations.dreamGateWondrousJourney) >>= \case
        Just wondrousJourney -> do
          currentLocation <- field InvestigatorLocation iid
          pointlessReality <- genCard Locations.dreamGatePointlessReality
          canLeaveCurrentLocation <- getCanLeaveCurrentLocation iid attrs
          swapLocation wondrousJourney pointlessReality
          for_ enemies (disengageEnemy iid)
          when (currentLocation /= Just wondrousJourney && canLeaveCurrentLocation) do
            moveTo (attrs.ability 1) iid wondrousJourney
        Nothing -> do
          for_ enemies (disengageEnemy iid)
          dreamGate <- placeLocationCard Locations.dreamGatePointlessReality
          checkAfter $ PutLocationIntoPlay iid dreamGate
          whenM (getCanLeaveCurrentLocation iid attrs)
            $ moveTo (attrs.ability 1) iid dreamGate
      pure t
    _ -> DetachedFromReality <$> liftRunMessage msg attrs
