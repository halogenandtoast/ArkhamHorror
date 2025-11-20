module Arkham.Scenario.Scenarios.DeadHeat (deadHeat) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Attack.Types
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Meta
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Act
import Arkham.Helpers.FlavorText
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard, randomDiscard)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Xp
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (LocationTokens))
import Arkham.Matcher hiding (enemyAt)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Story
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey
import Arkham.Scenarios.DeadHeat.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Strategy
import Arkham.Token
import Arkham.Window qualified as Window

newtype DeadHeat = DeadHeat ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deadHeat :: Difficulty -> DeadHeat
deadHeat difficulty =
  scenario
    DeadHeat
    "09520"
    "Dead Heat"
    difficulty
    [ "heart squiggle square"
    , ".     diamond   triangle"
    ]

instance HasChaosTokenValue DeadHeat where
  getChaosTokenValue iid tokenFace (DeadHeat attrs) = case tokenFace of
    Skull -> do
      n <- liftA2 div (scenarioCount CiviliansSlain) (perPlayer 1)
      pure $ toChaosTokenValue attrs Skull n (n * 2)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 4 6
    Tablet -> pure $ toChaosTokenValue attrs Tablet 1 2
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance HasModifiersFor DeadHeat where
  getModifiersFor (DeadHeat a) = do
    modifySelect a (LocationWithToken Civilian) [CountsAsInvestigatorForHunterEnemies]

instance RunMessage DeadHeat where
  runMessage msg s@(DeadHeat attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      n <- getTime
      flavor do
        setTitle "title"
        p "intro1"
        ul do
          li.validate (n < 15) "lessThan15"
          li.validate (n >= 15 && n <= 24) "between15And24"
          li.validate (n > 24) "moreThan24"
      when (n < 15) $ doStep 2 PreScenarioSetup
      when (n >= 15 && n <= 24) $ doStep 3 PreScenarioSetup
      when (n > 24) $ doStep 4 PreScenarioSetup
      setupKeys
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro2"
      pure s
    DoStep 3 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro3"
      pure s
    DoStep 4 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro4"
      push R5
      pure s
    Setup -> runScenarioSetup DeadHeat attrs do
      n <- getTime
      setup $ ul do
        li "gatherSets"
        li.nested "placeLocations" do
          li "beginPlay"
        li.nested "civilians" do
          li.validate (n <= 10) "tenOrFewerTime"
          li.validate (n >= 11 && n <= 17) "elevenToSeventeenTime"
          li.validate (n >= 18 && n <= 24) "eighteentoTwentyFourTime"
        li "setOutOfPlay"
        li.nested "decks" do
          li.validate (n < 15) "fewerThanFifteenTime"
          li.validate (n >= 15 && n <= 24) "fifteenToTwentyFourTime"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      gather Set.DeadHeat
      gather Set.ScarletSorcery
      gather Set.SpreadingCorruption
      gather Set.Ghouls
      gather Set.StrikingFear

      setAgendaDeck [Agendas.gnashingTeeth, Agendas.emptyStreets]
      if n >= 15
        then do
          setActDeck [Acts.ritualOfLifeAndDeath, Acts.queenOfNothingAtAll]
          doom <- perPlayer 2
          placeDoomOnAgenda doom
        else setActDeck [Acts.findAmaranth, Acts.ritualOfLifeAndDeath, Acts.queenOfNothingAtAll]

      startAt =<< place Locations.marrakeshRailwayStation
      placeAll [Locations.jemaaElFnaaSquare, Locations.saadiansTombs, Locations.tanneries]

      bahiaPalaceGardens <- place Locations.bahiaPalaceGardens

      setAside
        [ Locations.marrakeshRailwayStationAbandoned
        , Locations.jemaaElFnaaSquareAbandoned
        , Locations.saadiansTombsAbandoned
        , Locations.tanneriesAbandoned
        , Locations.bahiaPalaceGardensAbandoned
        , Enemies.amaranthLurkingCorruption
        , Enemies.razinFarhiReanimatedArtificer
        , Enemies.khalidBelovedCompanion
        , Enemies.ancientRaider
        , Enemies.ancientRaider
        , Keys.theLastBlossom
        , Stories.saveTheCivilians
        ]

      doStep 2 Setup

      when (n >= 15) do
        -- remove from set aside
        traverse_ fromSetAside [Enemies.amaranthLurkingCorruption, Keys.theLastBlossom]
        amaranth <- enemyAt Enemies.amaranthLurkingCorruption bahiaPalaceGardens
        createScarletKeyAt_ Keys.theLastBlossom (AttachedToEnemy amaranth)
        lead <- getLead
        saveTheCivilians <- fromSetAside Stories.saveTheCivilians
        resolveStoryWithPlacement lead saveTheCivilians Global
        khalid <- fromSetAside Enemies.khalidBelovedCompanion
        shuffleCardsIntoDeck Deck.EncounterDeck [khalid]
    DoStep 2 Setup -> do
      locations <- select Anywhere
      n <- perPlayer 1
      for_ locations $ placeTokensOn ScenarioSource Civilian (n + 1)
      time <- getTime
      lead <- getLead
      when (time >= 11 && time <= 17) do
        chooseNM lead n $ targets locations $ removeTokensOn ScenarioSource Civilian 1
      when (time >= 18 && time <= 24) do
        if n * 2 >= length locations
          then do
            for_ locations $ removeTokensOn ScenarioSource Civilian 1
            chooseNM lead ((n * 2) - length locations)
              $ targets locations
              $ removeTokensOn ScenarioSource Civilian 1
          else chooseNM lead (n * 2) $ targets locations $ removeTokensOn ScenarioSource Civilian 1
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          razin <- selectAny $ enemyIs Enemies.razinFarhiReanimatedArtificer
          resolutionFlavor $ scope "noResolution" do
            setTitle "title"
            p "body"
            ul do
              li "slay"
              li.nested "checkEnemies" do
                li.validate razin "razin"
                li.validate (not razin) "noRazin"
          selectEach (LocationWithToken Civilian) \location -> do
            civilians <- fieldMap LocationTokens (countTokens Civilian) location
            removeTokens ScenarioSource location Civilian civilians
            scenarioCountIncrementBy CiviliansSlain civilians
          currentAct <- getCurrentActStep
          when (currentAct == 1) do
            lead <- getLead
            saveTheCivilians <- fetchCard Stories.saveTheCivilians
            resolveStoryWithPlacement lead saveTheCivilians Global
            flipOverBy lead ScenarioSource (StoryId Stories.saveTheCivilians.cardCode)
          push $ if razin then R1 else R2
        Resolution 1 -> do
          setBearer Keys.theLastBlossom $ keyWithEnemy Enemies.amaranthLurkingCorruption
          record TheLoversAreReunited
          markTime 1
          resolutionWithXp "resolution1" $ allGainXp' attrs
          endOfScenario
        Resolution 2 -> do
          setBearer Keys.theLastBlossom $ keyWithEnemy Enemies.amaranthLurkingCorruption
          record YouHaventSeenTheLastOfAmaranth
          markTime 1
          resolutionWithXp "resolution2" $ allGainXp' attrs
          endOfScenario
        Resolution 3 -> do
          chooseBearer Keys.theLastBlossom
          record AmaranthHasLeftTheCoterie
          markTime 2
          resolutionWithXp "resolution3" $ allGainXp' attrs
          endOfScenario
        Resolution 4 -> do
          chooseBearer Keys.theLastBlossom
          record YouHaventSeenTheLastOfAmaranth
          markTime 1
          resolutionWithXp "resolution4" $ allGainXp' attrs
          endOfScenario
        Resolution 5 -> do
          record YouHaventSeenTheLastOfAmaranth
          eachInvestigator (`sufferMentalTrauma` 1)
          setBearer Keys.theLastBlossom $ keyWithEnemy Enemies.amaranthLurkingCorruption
          markTime 1
          resolutionWithXp "resolution5" $ allGainXpWithBonus' attrs (toBonus "bonus" 3)
          endOfScenario
        _ -> error "Unknown resolution for Dead Heat"
      pure s
    ScenarioSpecific "enemyAttackedAtLocation" v -> do
      let enemy :: EnemyId = toResult v
      checkWhen
        $ Window.EnemyAttacks
        $ EnemyAttackDetails
          { attackTarget = SingleAttackTarget (InvestigatorTarget "00000") -- lies
          , attackOriginalTarget = SingleAttackTarget (InvestigatorTarget "00000") -- lies
          , attackEnemy = enemy
          , attackType = RegularAttack
          , attackDamageStrategy = DamageAny
          , attackExhaustsEnemy = True
          , attackSource = EnemySource enemy
          , attackCanBeCanceled = False
          , attackAfter = []
          , attackDamaged = mempty
          , attackDealDamage = True
          , attackDespiteExhausted = True
          , attackCancelled = False
          }
      withLocationOf enemy slayCivilian
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist | isEasyStandard attrs -> do
          enemies <- select $ enemyAtLocationWith iid <> NonEliteEnemy
          chooseTargetM iid enemies \enemy -> do
            readyThis enemy
            initiateEnemyAttack enemy Cultist iid
        Tablet | isEasyStandard attrs -> do
          chooseOneM iid do
            unscoped $ countVar 1 $ labeled' "takeDamage" $ assignDamage iid Tablet 1
            labeled' "tablet.doNotTakeDamage" $ withLocationOf iid slayCivilian
        ElderThing | isEasyStandard attrs -> chooseAndDiscardCard iid ElderThing
        ElderThing | isHardExpert attrs -> randomDiscard iid ElderThing
        _ -> pure ()
      pure s
    ResolveChaosToken _ Cultist iid | isHardExpert attrs -> do
      enemies <- select $ enemyAtLocationWith iid <> NonEliteEnemy
      chooseTargetM iid enemies \enemy -> do
        readyThis enemy
        initiateEnemyAttack enemy Cultist iid
      pure s
    ResolveChaosToken _ Tablet iid | isHardExpert attrs -> do
      assignDamage iid Tablet 1
      withLocationOf iid slayCivilian
      pure s
    _ -> DeadHeat <$> liftRunMessage msg attrs
