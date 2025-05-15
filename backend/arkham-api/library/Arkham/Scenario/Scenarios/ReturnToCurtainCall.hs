module Arkham.Scenario.Scenarios.ReturnToCurtainCall (returnToCurtainCall) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Placement
import Arkham.Zone
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.CurtainCall
import Arkham.Scenarios.CurtainCall.Helpers

newtype ReturnToCurtainCall = ReturnToCurtainCall CurtainCall
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToCurtainCall :: Difficulty -> ReturnToCurtainCall
returnToCurtainCall difficulty =
  scenarioWith
    (ReturnToCurtainCall . CurtainCall)
    "52014"
    "Return to Curtain Call"
    difficulty
    [ "lobbyDoorway1 .     balcony .         backstageDoorway1"
    , "lobbyDoorway3 lobby theatre backstage backstageDoorway3"
    , "lobbyDoorway2 .     .       .         backstageDoorway2"
    ]
    (referenceL .~ "03043")

instance RunMessage ReturnToCurtainCall where
  runMessage msg (ReturnToCurtainCall curtainCall'@(CurtainCall attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup -> runScenarioSetup (ReturnToCurtainCall . CurtainCall) attrs do
      gather Set.ReturnToCurtainCall
      gather Set.CurtainCall
      gather Set.EvilPortents
      gather Set.MaddeningDelusions
      gather Set.Hauntings
      gather Set.CultOfTheYellowSign
      gather Set.NeuroticFear
      gather Set.Rats

      theatre <- place Locations.theatre
      backstage <- place Locations.backstage
      placeAll [Locations.lobby, Locations.balcony]

      investigators <- allInvestigators
      mLola <- selectOne $ InvestigatorWithTitle "Lola Hayes"
      for_ mLola \lola -> do
        chooseOneM lola do
          targeting backstage do
            reveal backstage
            moveTo_ attrs lola backstage

      let theatreInvestigators = maybe investigators (`deleteFirst` investigators) mLola

      unless (null theatreInvestigators) do
        lead <- getLead
        chooseOneM lead do
          targeting theatre do
            reveal theatre
            for_ theatreInvestigators \iid -> moveTo_ attrs iid theatre

      setAside
        [ Enemies.theManInThePallidMask
        , Locations.lightingBox
        , Locations.boxOffice
        , Locations.greenRoom
        , Locations.dressingRoom
        , Locations.rehearsalRoom
        , Locations.trapRoom
        ]

      royalEmissary <- placeEnemyCapture Enemies.royalEmissary (OutOfPlay SetAsideZone)
      push $ PlaceReferenceCard (toTarget royalEmissary) "52014b"

      setAgendaDeck [Agendas.theThirdAct, Agendas.encore]
      setActDeck
        [ Acts.awakening
        , Acts.theStrangerACityAflame
        , Acts.theStrangerThePathIsMine
        , Acts.theStrangerTheShoresOfHali
        , Acts.theStrangerAlaranMists
        , Acts.theStrangerUnderTheCity
        , Acts.theStrangerHereIsMyReply
        , Acts.curtainCall
        ]
    _ -> ReturnToCurtainCall <$> liftRunMessage msg curtainCall'
