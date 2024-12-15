module Arkham.Scenario.Scenarios.CurtainCall (CurtainCall (..), curtainCall) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.CampaignLogKey
import Arkham.Campaigns.ThePathToCarcosa.ChaosBag
import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers
import Arkham.Helpers.Investigator
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey
import Arkham.Scenarios.CurtainCall.Story
import Arkham.Token

newtype CurtainCall = CurtainCall ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

curtainCall :: Difficulty -> CurtainCall
curtainCall difficulty =
  scenario
    CurtainCall
    "03043"
    "Curtain Call"
    difficulty
    [ "lobbyDoorway1 .     balcony .         backstageDoorway1"
    , "lobbyDoorway3 lobby theatre backstage backstageDoorway3"
    , "lobbyDoorway2 .     .       .         backstageDoorway2"
    ]

instance HasChaosTokenValue CurtainCall where
  getChaosTokenValue iid chaosTokenFace (CurtainCall attrs) = case chaosTokenFace of
    Skull -> do
      horrorCount <- field InvestigatorHorror iid
      let easyStandardModifier = if horrorCount >= 3 then 3 else 1
      let hardExpertModifier = max 1 horrorCount
      pure $ toChaosTokenValue attrs Skull easyStandardModifier hardExpertModifier
    face
      | face `elem` [Cultist, Tablet, ElderThing] ->
          pure $ toChaosTokenValue attrs face 4 5
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage CurtainCall where
  runMessage msg s@(CurtainCall attrs) = runQueueT $ case msg of
    PreScenarioSetup -> do
      story intro
      pure s
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    Setup -> runScenarioSetup CurtainCall attrs do
      gather Set.CurtainCall
      gather Set.EvilPortents
      gather Set.Delusions
      gather Set.Hauntings
      gather Set.CultOfTheYellowSign
      gather Set.StrikingFear
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
        [ Enemies.royalEmissary
        , Enemies.theManInThePallidMask
        , Locations.lightingBox
        , Locations.boxOffice
        , Locations.greenRoom
        , Locations.dressingRoom
        , Locations.rehearsalRoom
        , Locations.trapRoom
        ]
      setAgendaDeck [Agendas.theThirdAct, Agendas.encore]
      setActDeck
        [ Acts.awakening
        , Acts.theStrangerACityAflame
        , Acts.theStrangerThePathIsMine
        , Acts.theStrangerTheShoresOfHali
        , Acts.curtainCall
        ]
    ScenarioResolution resolution -> do
      let stoleFromTheBoxOffice = member StoleFromTheBoxOffice attrs.log
      case resolution of
        NoResolution -> story noResolution
        Resolution 1 -> do
          story resolution1
          record YouTriedToWarnThePolice
          markConviction
          when stoleFromTheBoxOffice $ record ThePoliceAreSuspiciousOfYou
        Resolution 2 -> do
          story resolution2
          record YouChoseNotToGoToThePolice
          markDoubt
          when stoleFromTheBoxOffice $ record ThePoliceAreSuspiciousOfYou
        _ -> error "Invalid resolution"

      record TheStrangerIsOnToYou
      lead <- getLead
      addCampaignCardToDeck lead Enemies.theManInThePallidMask
      allGainXp attrs
      endOfScenario
      pure s
    ResolveChaosToken _ chaosTokenFace iid | chaosTokenFace `elem` [Cultist, Tablet, ElderThing] -> do
      withLocationOf iid \lid -> do
        horrorCount <- field LocationHorror lid
        if horrorCount > 0
          then assignHorror iid attrs 1
          else placeTokens chaosTokenFace lid Horror 1
      pure s
    _ -> CurtainCall <$> liftRunMessage msg attrs
