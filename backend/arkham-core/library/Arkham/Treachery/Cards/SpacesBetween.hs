module Arkham.Treachery.Cards.SpacesBetween (
  spacesBetween,
  SpacesBetween (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Movement
import Arkham.Projection
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype SpacesBetween = SpacesBetween TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spacesBetween :: TreacheryCard SpacesBetween
spacesBetween = treachery SpacesBetween Cards.spacesBetween

instance RunMessage SpacesBetween where
  runMessage msg t@(SpacesBetween attrs) = case msg of
    Revelation _ source | isSource attrs source -> do
      nonSentinelHillLocations <- selectList $ LocationWithoutTrait SentinelHill
      msgs <-
        concatMapM'
          ( \flipLocation -> do
              let locationMatcher = LocationWithId flipLocation
              investigatorIds <- selectList $ InvestigatorAt locationMatcher
              enemyIds <- selectList $ EnemyAt locationMatcher <> UnengagedEnemy
              destination <-
                fromJustNote "must be connected to a sentinel location"
                  <$> selectOne
                    ( AccessibleFrom locationMatcher
                        <> LocationWithTrait SentinelHill
                    )

              pure
                $ [MoveTo $ move source iid destination | iid <- investigatorIds]
                <> [EnemyMove eid destination | eid <- enemyIds]
                <> [UnrevealLocation flipLocation]
          )
          nonSentinelHillLocations

      alteredPaths <-
        shuffleM
          =<< filterM
            (fieldP LocationUnrevealedName (== "Altered Path"))
            nonSentinelHillLocations
      divergingPaths <-
        shuffleM
          =<< filterM
            (fieldP LocationUnrevealedName (== "Diverging Path"))
            nonSentinelHillLocations

      t
        <$ pushAll
          ( msgs
              <> [ SetLocationLabel locationId $ "alteredPath" <> tshow idx
                 | (idx, locationId) <- zip [1 :: Int ..] alteredPaths
                 ]
              <> [ SetLocationLabel locationId $ "divergingPath" <> tshow idx
                 | (idx, locationId) <- zip [1 :: Int ..] divergingPaths
                 ]
          )
    _ -> SpacesBetween <$> runMessage msg attrs
