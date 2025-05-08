module Arkham.Treachery.Cards.SpacesBetween (spacesBetween) where

import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SpacesBetween = SpacesBetween TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spacesBetween :: TreacheryCard SpacesBetween
spacesBetween = treachery SpacesBetween Cards.spacesBetween

instance RunMessage SpacesBetween where
  runMessage msg t@(SpacesBetween attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      nonSentinelHillLocations <- select $ LocationWithoutTrait SentinelHill

      for_ nonSentinelHillLocations \flipLocation -> do
        let locationMatcher = LocationWithId flipLocation
        mdestination <- selectOne $ ConnectedTo locationMatcher <> LocationWithTrait SentinelHill
        for_ mdestination \destination -> do
          selectEach (InvestigatorAt locationMatcher) \iid -> moveTo attrs iid destination
          selectEach (at_ locationMatcher <> UnengagedEnemy) (`enemyMoveTo` destination)
        removeAllClues attrs flipLocation
        push $ UnrevealLocation flipLocation

      alteredPaths <-
        shuffle
          =<< filterM (fieldP LocationUnrevealedName (== "Altered Path")) nonSentinelHillLocations
      divergingPaths <-
        shuffle
          =<< filterM (fieldP LocationUnrevealedName (== "Diverging Path")) nonSentinelHillLocations

      pushAll
        $ [ SetLocationLabel locationId $ "alteredPath" <> tshow idx
          | (idx, locationId) <- withIndex1 alteredPaths
          ]
        <> [ SetLocationLabel locationId $ "divergingPath" <> tshow idx
           | (idx, locationId) <- withIndex1 divergingPaths
           ]
      pure t
    _ -> SpacesBetween <$> liftRunMessage msg attrs
