module Arkham.Types.Treachery.Cards.SpacesBetween
  ( spacesBetween
  , SpacesBetween(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Card.CardDef
import Arkham.Types.Classes
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype SpacesBetween = SpacesBetween TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spacesBetween :: TreacheryCard SpacesBetween
spacesBetween = treachery SpacesBetween Cards.spacesBetween

instance TreacheryRunner env => RunMessage env SpacesBetween where
  runMessage msg t@(SpacesBetween attrs) = case msg of
    Revelation _ source | isSource attrs source -> do
      nonSentinelHillLocations <- selectList $ LocationWithoutTrait SentinelHill
      msgs <- concatMapM'
        (\flipLocation -> do
          let locationMatcher = LocationWithId flipLocation
          investigatorIds <- selectList $ InvestigatorAt locationMatcher
          enemyIds <- selectList $ EnemyAt locationMatcher <> UnengagedEnemy
          destination <-
            fromJustNote "must be connected to a sentinel location"
              <$> selectOne
                    (AccessibleFrom locationMatcher
                    <> LocationWithTrait SentinelHill
                    )

          -- TODO: We should not be removing the location
          pure
            $ [ MoveTo source iid destination | iid <- investigatorIds ]
            <> [ EnemyMove eid flipLocation destination | eid <- enemyIds ]
            <> [RemoveLocation flipLocation]
        )
        nonSentinelHillLocations
      shuffledLocations <- traverse (\lid -> (lid, ) <$> getCardDef lid)
        =<< shuffleM nonSentinelHillLocations
      t <$ pushAll
        (msgs
        <> [ PlaceLocation locationId cardDef
           | (locationId, cardDef) <- shuffledLocations
           ]
        )
    _ -> SpacesBetween <$> runMessage msg attrs
