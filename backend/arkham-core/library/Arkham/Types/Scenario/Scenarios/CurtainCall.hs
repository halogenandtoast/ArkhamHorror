module Arkham.Types.Scenario.Scenarios.CurtainCall
  ( CurtainCall(..)
  , curtainCall
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Acts
import qualified Arkham.Agenda.Cards as Agendas
import qualified Arkham.Enemy.Cards as Enemies
import qualified Arkham.Location.Cards as Locations
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Card.PlayerCard
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
        [Agendas.theThirdAct, Agendas.encore]
        [ Acts.awakening
        , Acts.theStrangerACityAflame
        , Acts.theStrangerThePathIsMine
        , Acts.theStrangerTheShoresOfHali
        , Acts.curtainCall
        ]
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
        [ EncounterSet.CurtainCall
        , EncounterSet.EvilPortents
        , EncounterSet.Delusions
        , EncounterSet.Hauntings
        , EncounterSet.StrikingFear
        , EncounterSet.Rats
        ]
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
      setAsidePlayerCards <-
        pure . PlayerCard <$> genPlayerCard Enemies.theManInThePallidMask
      setAsideEncounterCards <- traverse
        (fmap EncounterCard . genEncounterCard)
        [ Enemies.royalEmissary
        , Locations.lightingBox
        , Locations.boxOffice
        , Locations.greenRoom
        , Locations.dressingRoom
        , Locations.rehearsalRoom
        , Locations.trapRoom
        ]
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
        .~ (setAsidePlayerCards <> setAsideEncounterCards)
        )
    _ -> CurtainCall <$> runMessage msg attrs
