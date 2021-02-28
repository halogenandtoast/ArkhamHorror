module Arkham.Types.Treachery.Cards.SpacesBetween
  ( spacesBetween
  , SpacesBetween(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId

newtype SpacesBetween = SpacesBetween TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spacesBetween :: TreacheryId -> a -> SpacesBetween
spacesBetween uuid _ = SpacesBetween $ baseAttrs uuid "02297"

instance HasModifiersFor env SpacesBetween where
  getModifiersFor = noModifiersFor

instance HasActions env SpacesBetween where
  getActions i window (SpacesBetween attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env SpacesBetween where
  runMessage msg t@(SpacesBetween attrs@TreacheryAttrs {..}) = case msg of
    Revelation _iid source | isSource attrs source -> do
      sentinelHillLocations <- getSet @LocationId [SentinelHill]
      nonSentinelHillLocations <-
        setToList . (`difference` sentinelHillLocations) <$> getSet ()
      unengagedEnemies <- mapSet unUnengagedEnemyId <$> getSet ()
      msgs <- concatMapM'
        (\nonSentinelHillLocation -> do
          investigatorIds <- getSetList @InvestigatorId nonSentinelHillLocation
          enemyIds <-
            setToList
            . intersection unengagedEnemies
            <$> getSet nonSentinelHillLocation
          connectedSentinelHillLocation <-
            fromJustNote "must be connected to a sentinel hill location"
            . find (`member` sentinelHillLocations)
            . map unConnectedLocationId
            <$> getSetList nonSentinelHillLocation
          pure
            $ [ MoveTo iid connectedSentinelHillLocation
              | iid <- investigatorIds
              ]
            <> [ EnemyMove
                   eid
                   nonSentinelHillLocation
                   connectedSentinelHillLocation
               | eid <- enemyIds
               ]
            <> [RemoveLocation nonSentinelHillLocation]
        )
        nonSentinelHillLocations
      shuffledLocations <- shuffleM nonSentinelHillLocations
      t <$ unshiftMessages
        (msgs
        <> map PlaceLocation shuffledLocations
        <> [Discard (toTarget attrs)]
        )
    _ -> SpacesBetween <$> runMessage msg attrs
