module Arkham.Types.Scenario.Scenarios.CurtainCall
  ( CurtainCall(..)
  , curtainCall
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Enemies
import qualified Arkham.Location.Cards as Locations
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import Arkham.Types.Token

newtype CurtainCall = CurtainCall ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

curtainCall :: Difficulty -> CurtainCall
curtainCall difficulty =
  CurtainCall
    $ baseAttrs
        "03043"
        "Curtain Call"
        ["03044", "03045"]
        ["03046", "03047a", "03047b", "03047c", "03048"]
        difficulty
    & locationLayoutL
    ?~ [ "lobbyDoorway1 .     balcony .         backstageDoorway1"
       , "lobbyDoorway3 lobby theatre backstage backstageDoorway3"
       , "lobbyDoorway2 .     .       .         backstageDoorway2"
       ]

instance HasRecord CurtainCall where
  hasRecord _ = pure False
  hasRecordSet _ = pure []
  hasRecordCount _ = pure 0

instance
  ( HasTokenValue env InvestigatorId
  , HasCount HorrorCount env InvestigatorId
  )
  => HasTokenValue env CurtainCall where
  getTokenValue (CurtainCall attrs) iid = \case
    Skull -> do
      horrorCount <- unHorrorCount <$> getCount iid
      let easyStandardModifier = if horrorCount >= 3 then 3 else 1
      let hardExpertModifier = max 1 horrorCount
      pure $ toTokenValue attrs Skull easyStandardModifier hardExpertModifier
    face | face `elem` [Cultist, Tablet, ElderThing] ->
      pure $ toTokenValue attrs face 4 5
    otherFace -> getTokenValue attrs iid otherFace

instance ScenarioRunner env => RunMessage env CurtainCall where
  runMessage msg (CurtainCall attrs) = case msg of
    Setup -> do
      encounterDeck <- buildEncounterDeckExcluding
        [Enemies.royalEmissary]
        [EncounterSet.CurtainCall, EncounterSet.StrikingFear, EncounterSet.Rats]
      theatreId <- getRandom
      lobbyId <- getRandom
      balconyId <- getRandom
      backstageId <- getRandom

      investigatorIds <- getInvestigatorIds
      mLolaId <- selectOne $ InvestigatorWithTitle "Lola Hayes"
      let
        theatreInvestigatorIds =
          maybe investigatorIds (`deleteFirst` investigatorIds) mLolaId
        theatreMoveTo = map
          (\iid -> MoveTo (toSource attrs) iid theatreId)
          theatreInvestigatorIds
        backstageMoveTo =
          [ MoveTo (toSource attrs) lolaId backstageId
          | lolaId <- maybeToList mLolaId
          ]

      pushAll
        ([ SetEncounterDeck encounterDeck
         , AddAgenda "03044"
         , AddAct "03046"
         , PlaceLocation theatreId Locations.theatre
         , PlaceLocation lobbyId Locations.lobby
         , PlaceLocation balconyId Locations.balcony
         , PlaceLocation backstageId Locations.backstage
         ]
        <> theatreMoveTo
        <> backstageMoveTo
        )
      backstageDoorways <- traverse
        (fmap EncounterCard . genEncounterCard)
        [Locations.dressingRoom, Locations.rehearsalRoom, Locations.trapRoom]
      lobbyDoorways <- traverse
        (fmap EncounterCard . genEncounterCard)
        [Locations.lightingBox, Locations.boxOffice, Locations.greenRoom]
      CurtainCall <$> runMessage
        msg
        (attrs
        & locationsL
        .~ locationNameMap
             [ Locations.theatre
             , Locations.lobby
             , Locations.balcony
             , Locations.backstage
             ]
        & setAsideCardsL
        .~ backstageDoorways
        <> lobbyDoorways
        )
    _ -> CurtainCall <$> runMessage msg attrs
