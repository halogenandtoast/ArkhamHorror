module Arkham.Treachery.Cards.DetachedFromReality (
  detachedFromReality,
  DetachedFromReality (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Movement
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype DetachedFromReality = DetachedFromReality TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

detachedFromReality :: TreacheryCard DetachedFromReality
detachedFromReality = treachery DetachedFromReality Cards.detachedFromReality

instance RunMessage DetachedFromReality where
  runMessage msg t@(DetachedFromReality attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      mWondrousJourney <- selectOne $ locationIs Locations.dreamGateWondrousJourney
      enemies <- selectList $ enemyEngagedWith iid
      case mWondrousJourney of
        Just wondrousJourney -> do
          currentLocation <- field InvestigatorLocation iid
          pointlessReality <- genCard Locations.dreamGatePointlessReality
          pushAll $ ReplaceLocation wondrousJourney pointlessReality Swap
            : map (DisengageEnemy iid) enemies
              <> [ MoveTo $ move (toAbilitySource attrs 1) iid wondrousJourney
                 | currentLocation /= Just wondrousJourney
                 ]
        Nothing -> do
          (dreamGate, placement) <- placeLocationCard Locations.dreamGatePointlessReality
          pushAll
            $ map (DisengageEnemy iid) enemies
            <> [placement, MoveTo $ move (toAbilitySource attrs 1) iid dreamGate]
      pure t
    _ -> DetachedFromReality <$> runMessage msg attrs
