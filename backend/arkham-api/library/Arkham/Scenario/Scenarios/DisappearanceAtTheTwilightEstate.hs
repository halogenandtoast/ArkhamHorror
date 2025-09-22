module Arkham.Scenario.Scenarios.DisappearanceAtTheTwilightEstate (disappearanceAtTheTwilightEstate) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Types (Field (..))
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Campaigns.TheCircleUndone.ChaosBag
import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Campaigns.TheCircleUndone.Key
import Arkham.Card
import Arkham.Decklist
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (allPlayers)
import Arkham.Helpers.SkillTest
import Arkham.Id
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (enemyAt)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move
import Arkham.Name (toTitle)
import Arkham.Placement
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.DisappearanceAtTheTwilightEstate.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype DisappearanceAtTheTwilightEstate = DisappearanceAtTheTwilightEstate ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disappearanceAtTheTwilightEstate
  :: Difficulty -> DisappearanceAtTheTwilightEstate
disappearanceAtTheTwilightEstate difficulty =
  scenario
    DisappearanceAtTheTwilightEstate
    "05043"
    "Disappearance at the Twilight Estate"
    difficulty
    [ ".             .          office         .             ."
    , "billiardsRoom trophyRoom victorianHalls masterBedroom balcony"
    , ".             .          entryHall      .             ."
    ]

instance HasChaosTokenValue DisappearanceAtTheTwilightEstate where
  getChaosTokenValue iid chaosTokenFace (DisappearanceAtTheTwilightEstate attrs) =
    case chaosTokenFace of
      Skull -> pure $ toChaosTokenValue attrs Skull 3 5
      otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage DisappearanceAtTheTwilightEstate where
  runMessage msg s@(DisappearanceAtTheTwilightEstate attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "body"
      -- investigators have not been chosen yet so we have to send to players
      allPlayers >>= traverse_ (push . (`ForPlayer` msg))
      flavor $ setTitle "title" >> p "intro"
      doStep 2 msg
      pure s
    ForPlayer player PreScenarioSetup -> scope "intro" do
      taken <- select Anyone
      let
        availablePrologueInvestigators =
          filter
            ((`notElem` taken) . InvestigatorId . cdCardCode)
            allPrologueInvestigators
      playerChooseOneM player do
        questionLabeled' "chooseInvestigator"
        cardsLabeled availablePrologueInvestigators \card -> do
          push
            $ LoadDecklist player
            $ ArkhamDBDecklist
              mempty
              mempty
              (InvestigatorId $ cdCardCode card)
              (toTitle card)
              Nothing
              Nothing
              Nothing -- TODO: should we figure out the taboo list here??
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro" do
      taken <- selectMap unInvestigatorId Anyone
      let
        prologueInvestigatorsNotTaken =
          map cdCardCode allPrologueInvestigators
            \\ toList taken
        readingFor = \case
          "05046" -> "gavriellaIntro"
          "05047" -> "jeromeIntro"
          "05048" -> "valentinoIntro"
          "05049" -> "pennyIntro"
          _ -> error "Invalid prologue investigator"
        readings = map readingFor taken
      crossOutRecordSetEntries MissingPersons prologueInvestigatorsNotTaken
      traverse_ (\r -> flavor $ setTitle "title" >> p r) readings
      pure s
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    Setup -> runScenarioSetup DisappearanceAtTheTwilightEstate attrs do
      -- At Death's Doorstep is only locations so we will manually gather
      gather Set.DisappearanceAtTheTwilightEstate
      gather Set.InexorableFate
      gather Set.RealmOfDeath
      gather Set.SpectralPredators
      gather Set.TrappedSpirits
      gather Set.TheWatcher
      gather Set.ChillingCold

      setActDeck [Acts.theDisappearance]
      setAgendaDeck [Agendas.judgementXX]

      office <- place Locations.officeSpectral
      billiardsRoom <- place Locations.billiardsRoomSpectral
      victorianHalls <- place Locations.victorianHallsSpectral
      balcony <- place Locations.balconySpectral
      entryHall <- place Locations.entryHallSpectral

      placeAll [Locations.trophyRoomSpectral, Locations.masterBedroomSpectral]

      enemyAt_ Enemies.theSpectralWatcher entryHall

      selectForMaybeM (investigatorIs Investigators.gavriellaMizrah) \gavriella -> do
        moveTo_ attrs gavriella victorianHalls
        removeOneOf Treacheries.fateOfAllFools

      selectForMaybeM (investigatorIs Investigators.jeromeDavids) \jerome -> do
        moveTo_ attrs jerome office
        tid1 <- getRandom
        obscuringFog <- genCard Treacheries.obscuringFog
        push $ AttachStoryTreacheryTo tid1 obscuringFog (toTarget office)
        enemyAt_ Enemies.netherMist office

      selectForMaybeM (investigatorIs Investigators.valentinoRivas) \valentino -> do
        moveTo_ attrs valentino billiardsRoom
        placeEnemy Enemies.shadowHound (InThreatArea valentino)
        removeOneOf Treacheries.terrorInTheNight

      selectForMaybeM (investigatorIs Investigators.pennyWhite) \penny -> do
        moveTo_ attrs penny balcony
        placeEnemy Enemies.wraith (InThreatArea penny)
        removeOneOf Treacheries.whispersInTheDark

      push $ SetupStep (toTarget attrs) 2
    SetupStep (isTarget attrs -> True) 2 -> do
      agendaId <- selectJust AnyAgenda

      whenAny (investigatorIs Investigators.gavriellaMizrah) do
        theSpectralWatcher <- selectJust $ enemyIs Enemies.theSpectralWatcher
        nonAttackEnemyDamage Nothing attrs 1 theSpectralWatcher

      whenAny (investigatorIs Investigators.valentinoRivas) do
        terrorInTheNight <- genCard Treacheries.terrorInTheNight
        tid1 <- getRandom
        push $ AttachStoryTreacheryTo tid1 terrorInTheNight (toTarget agendaId)

      whenAny (investigatorIs Investigators.pennyWhite) do
        whispersInTheDark <- genCard Treacheries.whispersInTheDark
        tid2 <- getRandom
        push $ AttachStoryTreacheryTo tid2 whispersInTheDark (toTarget agendaId)
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Skull ->
          getSkillTestAction >>= traverse_ \action ->
            when (action `elem` [#fight, #evade]) $ runHauntedAbilities iid
        _ -> pure ()
      pure s
    ScenarioResolution _ -> scope "resolutions" do
      resolution "noResolution"
      recordCount PiecesOfEvidenceWereLeftBehind =<< selectSum ActClues AnyAct
      endOfScenario
      pure s
    _ -> DisappearanceAtTheTwilightEstate <$> liftRunMessage msg attrs
