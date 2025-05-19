module Arkham.Scenario.Scenarios.CurtainCall (setupCurtainCall, curtainCall, CurtainCall (..)) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Campaigns.ThePathToCarcosa.Import
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location
import Arkham.Helpers.Query
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey
import Arkham.Scenarios.CurtainCall.Helpers
import Arkham.Token
import Arkham.Zone

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

setupCurtainCall :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> ScenarioBuilderT m ()
setupCurtainCall attrs = do
  setup do
    ul do
      li "gatherSets"
      li "setAside"
      li.nested "placeLocations.instructions" do
        li "placeLocations.lola"
      unscoped $ li "shuffleRemainder"

  whenReturnTo $ gather Set.ReturnToCurtainCall
  gather Set.CurtainCall
  gather Set.EvilPortents
  gather Set.Delusions `orWhenReturnTo` gather Set.MaddeningDelusions
  gather Set.Hauntings
  gather Set.CultOfTheYellowSign
  gather Set.StrikingFear `orWhenReturnTo` gather Set.NeuroticFear
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

  whenReturnTo $ setAside [Locations.theatreLounge, Locations.propShop]

  royalEmissary <- placeEnemyCapture Enemies.royalEmissary (OutOfPlay SetAsideZone)
  whenReturnTo $ push $ PlaceReferenceCard (toTarget royalEmissary) "52014b"

  setAgendaDeck [Agendas.theThirdAct, Agendas.encore]
  isReturnTo <- getIsReturnTo

  setActDeck
    $ [ Acts.awakening
      , Acts.theStrangerACityAflame
      , Acts.theStrangerThePathIsMine
      , Acts.theStrangerTheShoresOfHali
      ]
    <> ( guard isReturnTo
           *> [Acts.theStrangerAlaranMists, Acts.theStrangerUnderTheCity, Acts.theStrangerHereIsMyReply]
       )
    <> [Acts.curtainCall]

instance RunMessage CurtainCall where
  runMessage msg s@(CurtainCall attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      flavor $ scope "intro" $ h "title" >> p "body"
      pure s
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    Setup -> runScenarioSetup CurtainCall attrs $ setupCurtainCall attrs
    ScenarioResolution r -> scope "resolutions" do
      let stoleFromTheBoxOffice = member StoleFromTheBoxOffice attrs.log
      case r of
        NoResolution -> resolutionWithXp "noResolution" $ allGainXp' attrs
        Resolution 1 -> do
          resolutionWithXp "resolution1" $ allGainXp' attrs
          record YouTriedToWarnThePolice
          markConviction
          when stoleFromTheBoxOffice $ record ThePoliceAreSuspiciousOfYou
        Resolution 2 -> do
          resolutionWithXp "resolution2" $ allGainXp' attrs
          record YouChoseNotToGoToThePolice
          markDoubt
          when stoleFromTheBoxOffice $ record ThePoliceAreSuspiciousOfYou
        _ -> error "Invalid resolution"

      record TheStrangerIsOnToYou
      lead <- getLead
      addCampaignCardToDeck lead DoNotShuffleIn Enemies.theManInThePallidMask
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
