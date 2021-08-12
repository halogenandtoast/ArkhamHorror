module Arkham.Types.Treachery.Cards.SpacesBetween
  ( spacesBetween
  , SpacesBetween(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Card.CardDef
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype SpacesBetween = SpacesBetween TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spacesBetween :: TreacheryCard SpacesBetween
spacesBetween = treachery SpacesBetween Cards.spacesBetween

instance (GetCardDef env LocationId, TreacheryRunner env) => RunMessage env SpacesBetween where
  runMessage msg t@(SpacesBetween attrs) = case msg of
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
            $ concat
                [ [ MoveTo iid connectedSentinelHillLocation
                  , MovedBy iid (toSource attrs)
                  ]
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
      shuffledLocations <- traverse (\lid -> (lid, ) <$> getCardDef lid)
        =<< shuffleM nonSentinelHillLocations
      t <$ pushAll
        (msgs
        <> [ PlaceLocation locationId cardDef
           | (locationId, cardDef) <- shuffledLocations
           ]
        <> [Discard (toTarget attrs)]
        )
    _ -> SpacesBetween <$> runMessage msg attrs
