module Arkham.Scenario.Scenarios.DisappearanceAtTheTwilightEstate (
  setupDisappearanceAtTheTwilightEstate,
  disappearanceAtTheTwilightEstate,
  DisappearanceAtTheTwilightEstate (..),
) where

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
import Arkham.Helpers.Scenario qualified as Scenario
import Arkham.Helpers.SkillTest
import Arkham.I18n
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
import Arkham.Scenario.Types (ScenarioAttrs (scenarioTarotDeck))
import Arkham.Scenarios.DisappearanceAtTheTwilightEstate.Helpers
import Arkham.Tarot
import Arkham.Treachery.Cards qualified as Treacheries
import Data.List.NonEmpty qualified as NE

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

setupDisappearanceAtTheTwilightEstate
  :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> ScenarioBuilderT m ()
setupDisappearanceAtTheTwilightEstate attrs = do
  -- At Death's Doorstep is only locations so we will manually gather
  setup do
    ul do
      li "gatherSets"
      li "placeLocations"
      li "theSpectralWatcher"
      li "investigatorSetup"
      unscoped $ li "shuffleRemainder"
    p "theSpectralRealm"

  scope "noWayOut" $ flavor $ h "title" >> p "body"

  whenReturnTo $ gather Set.ReturnToDisappearanceAtTheTwilightEstate
  gather Set.DisappearanceAtTheTwilightEstate
  gather Set.InexorableFate `orWhenReturnTo` gather Set.UnspeakableFate
  gather Set.RealmOfDeath `orWhenReturnTo` gather Set.UnstableRealm
  gather Set.SpectralPredators
  gather Set.TrappedSpirits `orWhenReturnTo` gather Set.BloodthirstySpirits
  gather Set.TheWatcher
  gather Set.ChillingCold `orWhenReturnTo` gather Set.ChillingMists

  setActDeck [Acts.theDisappearance]
  setAgendaDeck [Agendas.judgementXX]

  office <- place Locations.officeSpectral
  billiardsRoom <- place Locations.billiardsRoomSpectral
  victorianHalls <- place Locations.victorianHallsSpectral
  balcony <- place Locations.balconySpectral
  entryHall <- place Locations.entryHallSpectral

  placeAll [Locations.trophyRoomSpectral, Locations.masterBedroomSpectral]

  whenReturnTo $ place_ Locations.wineCellarSpectral

  enemyAt_ Enemies.theSpectralWatcher entryHall

  isReturnTo <- getIsReturnTo

  selectForMaybeM (investigatorIs Investigators.gavriellaMizrah) \gavriella -> do
    moveTo_ attrs gavriella victorianHalls
    removeOneOf
      $ if isReturnTo then Treacheries.fateOfAllFoolsUnspeakableFate else Treacheries.fateOfAllFools

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
  whenReturnTo $ addAdditionalReferences ["54016b"]

instance RunMessage DisappearanceAtTheTwilightEstate where
  runMessage msg s@(DisappearanceAtTheTwilightEstate attrs) = runQueueT $ scenarioI18n $ case msg of
    LoadScenario _ -> scope "intro" do
      isReturnTo <- Scenario.getIsReturnTo
      if isReturnTo
        then do
          allPlayers >>= \case
            [] -> error "no players"
            (player : _) -> do
              playerStoryWithChooseOneM' player (setTitle "title" >> p "returnToChoice") do
                labeled' "returnToSkip" $ doStep 3 msg
                labeled' "playNormally" $ doStep 1 msg
        else doStep 1 msg
      pure s
    DoStep 1 (LoadScenario opts) -> do
      result <- liftRunMessage (LoadScenario opts) attrs
      pure $ DisappearanceAtTheTwilightEstate $ result & startedL .~ False
    DoStep 3 (LoadScenario opts) -> scope "intro" do
      when (null $ scenarioTarotDeck attrs) $ push LoadTarotDeck
      allPlayers >>= traverse_ (push . (`ForPlayer` msg))
      doStep 4 (LoadScenario opts)
      endOfScenario
      pure s
    DoStep 4 (LoadScenario _opts) -> do
      taken <- selectMap unInvestigatorId Anyone
      let
        prologueInvestigatorsNotTaken =
          map cdCardCode allPrologueInvestigators
            \\ toList taken
      crossOutRecordSetEntries MissingPersons prologueInvestigatorsNotTaken
      pure s
    ForPlayer player (DoStep 3 (LoadScenario opts)) -> scope "intro" do
      taken <- select Anyone
      let
        availablePrologueInvestigators =
          filter
            ((`notElem` taken) . InvestigatorId . cdCardCode)
            allPrologueInvestigators
      playerStoryWithChooseOneM' player (setTitle "title" >> p "chooseInvestigator") do
        cardsLabeled availablePrologueInvestigators \card -> do
          -- we load the decklist just to remove the option
          let iid = InvestigatorId (cdCardCode card)
          push
            $ LoadDecklist player
            $ ArkhamDBDecklist mempty mempty iid (toTitle card) Nothing Nothing Nothing
          push $ DrawAndChooseTarot iid Upright 1
          forInvestigator iid (DoStep 3 (LoadScenario opts))
      pure s
    ForInvestigator iid (DoStep 3 (LoadScenario _)) -> scope "intro" do
      arcana <- sample (NE.fromList $ scenarioTarotDeck attrs)
      let card = fromJustNote "Failed" $ find ((== unInvestigatorId iid) . toCardCode) allPrologueInvestigators
      player <- getPlayer iid

      nameVar card $ playerStoryWithChooseOneM'
        player
        (setTitle "title" >> tarot arcana >> img card >> p (tshow arcana))
        do
          unscoped $ labeled' "continue" nothing

      case arcana of
        TheFool0 -> do
          recordSetInsert DisappearedIntoTheMist [unInvestigatorId iid]
          incrementRecordCount PiecesOfEvidenceWereLeftBehind 2
        TheMagicianI -> recordSetInsert DisappearedIntoTheMist [unInvestigatorId iid]
        TheHighPriestessII -> recordSetInsert DisappearedIntoTheMist [unInvestigatorId iid]
        TheEmpressIII -> do
          recordSetInsert WasClaimedBySpecters [unInvestigatorId iid]
          incrementRecordCount PiecesOfEvidenceWereLeftBehind 3
        TheEmperorIV -> do
          recordSetInsert WasClaimedBySpecters [unInvestigatorId iid]
          incrementRecordCount PiecesOfEvidenceWereLeftBehind 3
        TheHierophantV -> do
          recordSetInsert WasClaimedBySpecters [unInvestigatorId iid]
          incrementRecordCount PiecesOfEvidenceWereLeftBehind 3
        TheLoversVI -> do
          recordSetInsert WasClaimedBySpecters [unInvestigatorId iid]
          incrementRecordCount PiecesOfEvidenceWereLeftBehind 3
        TheChariotVII -> do
          recordSetInsert WasClaimedBySpecters [unInvestigatorId iid]
          incrementRecordCount PiecesOfEvidenceWereLeftBehind 5
        StrengthVIII -> do
          recordSetInsert WasClaimedBySpecters [unInvestigatorId iid]
          incrementRecordCount PiecesOfEvidenceWereLeftBehind 5
        TheHermitIX -> do
          recordSetInsert WasClaimedBySpecters [unInvestigatorId iid]
          incrementRecordCount PiecesOfEvidenceWereLeftBehind 5
        WheelOfFortuneX -> do
          recordSetInsert WasPulledIntoTheSpectralRealm [unInvestigatorId iid]
          incrementRecordCount PiecesOfEvidenceWereLeftBehind 2
        JusticeXI -> do
          recordSetInsert WasPulledIntoTheSpectralRealm [unInvestigatorId iid]
          incrementRecordCount PiecesOfEvidenceWereLeftBehind 2
        TheHangedManXII -> do
          recordSetInsert WasPulledIntoTheSpectralRealm [unInvestigatorId iid]
          incrementRecordCount PiecesOfEvidenceWereLeftBehind 2
        DeathXIII -> do
          recordSetInsert WasPulledIntoTheSpectralRealm [unInvestigatorId iid]
          incrementRecordCount PiecesOfEvidenceWereLeftBehind 4
        TemperanceXIV -> do
          recordSetInsert WasPulledIntoTheSpectralRealm [unInvestigatorId iid]
          incrementRecordCount PiecesOfEvidenceWereLeftBehind 4
        TheDevilXV -> do
          recordSetInsert WasPulledIntoTheSpectralRealm [unInvestigatorId iid]
          incrementRecordCount PiecesOfEvidenceWereLeftBehind 4
        TheTowerXVI -> do
          recordSetInsert WasPulledIntoTheSpectralRealm [unInvestigatorId iid]
          incrementRecordCount PiecesOfEvidenceWereLeftBehind 4
        TheStarXVII -> do
          recordSetInsert WasPulledIntoTheSpectralRealm [unInvestigatorId iid]
          incrementRecordCount PiecesOfEvidenceWereLeftBehind 6
        TheMoonXVIII -> do
          recordSetInsert WasPulledIntoTheSpectralRealm [unInvestigatorId iid]
          incrementRecordCount PiecesOfEvidenceWereLeftBehind 6
        TheSunXIX -> do
          recordSetInsert WasTakenByTheWatcher [unInvestigatorId iid]
          incrementRecordCount PiecesOfEvidenceWereLeftBehind 3
        JudgementXX -> do
          recordSetInsert WasTakenByTheWatcher [unInvestigatorId iid]
          incrementRecordCount PiecesOfEvidenceWereLeftBehind 3
        TheWorldXXI -> do
          recordSetInsert WasTakenByTheWatcher [unInvestigatorId iid]
          incrementRecordCount PiecesOfEvidenceWereLeftBehind 5
      pure $ DisappearanceAtTheTwilightEstate $ attrs & tarotDeckL %~ filter (/= arcana)
    PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "body"
      -- investigators have not been chosen yet so we have to send to players
      allPlayers >>= traverse_ (push . (`ForPlayer` msg))
      -- Now that investigators have been chosen we need to set the player order
      push SetPlayerOrder
      doStep 2 msg
      pure s
    ForPlayer player PreScenarioSetup -> scope "intro" do
      taken <- select Anyone
      let
        availablePrologueInvestigators =
          filter
            ((`notElem` taken) . InvestigatorId . cdCardCode)
            allPrologueInvestigators
      playerStoryWithChooseOneM' player (setTitle "title" >> p "chooseInvestigator") do
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
      flavor $ setTitle "title" >> p "intro"
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
      pure $ DisappearanceAtTheTwilightEstate $ attrs & startedL .~ True
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    Setup ->
      runScenarioSetup DisappearanceAtTheTwilightEstate attrs
        $ setupDisappearanceAtTheTwilightEstate attrs
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
