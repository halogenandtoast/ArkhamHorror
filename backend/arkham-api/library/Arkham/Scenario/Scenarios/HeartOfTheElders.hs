module Arkham.Scenario.Scenarios.HeartOfTheElders (
  HeartOfTheEldersMetadata (..),
  HeartOfTheEldersScenarioStep (..),
  setupHeartOfTheElders,
  heartOfTheElders,
  HeartOfTheElders (..),
) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Key
import Arkham.Card
import Arkham.Effect.Window
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.GameT (GameT)
import Arkham.Helpers (Deck (..))
import Arkham.Helpers.Act
import Arkham.Helpers.Campaign
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario hiding (getIsReturnTo)
import Arkham.Helpers.Tokens
import Arkham.Layout
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (enemyAt)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.Queue (QueueT)
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted hiding (EnemyDamage)
import Arkham.Scenario.Types (Field (ScenarioVictoryDisplay))
import Arkham.Scenarios.HeartOfTheElders.Story
import Arkham.Scenarios.HeartOfTheElders.Helpers
import Arkham.Token
import Arkham.Trait (Trait (Cave))
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window qualified as Window

data HeartOfTheEldersScenarioStep = One | Two
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data HeartOfTheEldersMetadata = HeartOfTheEldersMetadata
  { scenarioStep :: HeartOfTheEldersScenarioStep
  , reachedAct2 :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype HeartOfTheElders = HeartOfTheElders (ScenarioAttrs `With` HeartOfTheEldersMetadata)
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heartOfTheElders :: Difficulty -> HeartOfTheElders
heartOfTheElders difficulty =
  scenario
    (HeartOfTheElders . (`with` HeartOfTheEldersMetadata One False))
    "04205"
    "Heart of the Elders"
    difficulty
    [ ".        .        circle    circle    .     ."
    , ".        .        circle    circle    .     ."
    , "square   square   diamond   diamond   moon  moon"
    , "square   square   diamond   diamond   moon  moon"
    , ".        triangle triangle  heart     heart ."
    , ".        triangle triangle  heart     heart ."
    , "squiggle squiggle hourglass hourglass t     t"
    , "squiggle squiggle hourglass hourglass t     t"
    , ".        .        equals    equals    .     ."
    , ".        .        equals    equals    .     ."
    ]

part2Locations :: [GridTemplateRow]
part2Locations =
  [ ".      diamond  .      ."
  , ".      diamond  moon   ."
  , "equals circle   moon   heart"
  , "equals circle   square heart"
  , ".      triangle square ."
  , ".      triangle .      ."
  ]

standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
  [ PlusOne
  , Zero
  , Zero
  , Zero
  , MinusOne
  , MinusTwo
  , MinusTwo
  , MinusThree
  , MinusFive
  , Skull
  , Skull
  , Cultist
  , Tablet
  , ElderThing
  , AutoFail
  , ElderSign
  ]

instance HasChaosTokenValue HeartOfTheElders where
  getChaosTokenValue iid chaosTokenFace (HeartOfTheElders (attrs `With` _)) =
    case chaosTokenFace of
      Skull -> do
        inCave <- selectAny $ locationWithInvestigator iid <> LocationWithTrait Cave
        let caveModifier = if inCave then 2 else 0
        pure $ toChaosTokenValue attrs Skull (1 + caveModifier) (2 + caveModifier)
      Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
      Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
      ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
      otherFace -> getChaosTokenValue iid otherFace attrs

setupHeartOfTheElders
  :: ReverseQueue m => HeartOfTheEldersMetadata -> ScenarioAttrs -> ScenarioBuilderT m ()
setupHeartOfTheElders metadata attrs = scenarioI18n $ case scenarioStep metadata of
  One -> scope "part1" do
    pathsKnown <- getRecordCount PathsAreKnownToYou

    if pathsKnown == 6
      then do
        setup $ ul $ li.valid "pathsKnownToYou"
        push R1
      else do
        setup do
          ul do
            li.invalid "pathsKnownToYou"
            li "gatherSets"
            li.nested "placeLocations" do
              li "insightIntoHowToEnterKnYan"
            li "playedBefore"
            li "explorationDeck"
            li.nested "chooseLocations" do
              li "mappedOutTheWayForward"
            li "poisoned"
            unscoped $ li "shuffleRemainder"

        whenReturnTo do
          gather Set.ReturnToHeartOfTheElders
          gather Set.ReturnToPillarsOfJudgement
          gather Set.ReturnToRainforest
        gather Set.PillarsOfJudgement
        gather Set.HeartOfTheElders
        gather Set.Rainforest
        gather Set.Serpents
        gather Set.Expedition `orWhenReturnTo` gather Set.DoomedExpedition
        gather Set.Poison

        mouthOfKnYanTheCavernsMaw <- place Locations.mouthOfKnYanTheCavernsMaw
        startAt mouthOfKnYanTheCavernsMaw
        placeTokens attrs mouthOfKnYanTheCavernsMaw Pillar pathsKnown

        (ruinsLocation, toRemove) <-
          sampleWithRest $ Locations.overgrownRuins :| [Locations.templeOfTheFang, Locations.stoneAltar]

        removeOneOfEach toRemove

        mappedOutTheWayForward <- getHasRecord TheInvestigatorsMappedOutTheWayForward
        when mappedOutTheWayForward $ place_ ruinsLocation

        square <- Locations.pathOfThorns `orSampleIfReturnTo` [Locations.riversideTemple]
        moon <- Locations.ropeBridge `orSampleIfReturnTo` [Locations.waterfall]
        triangle <- Locations.serpentsHaven `orSampleIfReturnTo` [Locations.trailOfTheDead]
        heart <- Locations.circuitousTrail `orSampleIfReturnTo` [Locations.cloudForest]

        isReturnTo <- getIsReturnTo
        let
          treacheries =
            guard (not isReturnTo)
              *> [ Treacheries.pitfall
                 , Treacheries.ants
                 , Treacheries.lostInTheWilds
                 , Treacheries.lowOnSupplies
                 ]

        addExtraDeck ExplorationDeck
          =<< shuffle
            ( [ruinsLocation | not mappedOutTheWayForward]
                <> [ Locations.timeWrackedWoods
                   , square
                   , Locations.riverCanyon
                   , moon
                   , triangle
                   , heart
                   ]
                <> treacheries
            )

        setAsidePoisonedCount <- getSetAsidePoisonedCount
        setAside $ replicate setAsidePoisonedCount Treacheries.poisoned

        when (reachedAct2 metadata) do
          enemyAt_ Enemies.theWingedSerpent mouthOfKnYanTheCavernsMaw

        let act1 =
              if isReturnTo
                then Acts.aFamiliarPattern
                else Acts.searchForThePattern

        setActDeck $ [act1 | not (reachedAct2 metadata)] <> [Acts.openingTheMaw]
        setAgendaDeck [Agendas.theJunglesHeart, Agendas.settingSun]

        whenReturnTo do
          setAside [Enemies.harbingerOfValusiaTheSleeperReturns]
          addAdditionalReferences ["53045b"]
          createAbilityEffect EffectGameWindow
            $ mkAbility (SourceableWithCardCode (CardCode "53045b") ScenarioSource) 1
            $ forced
            $ Explored #after Anyone Anywhere (SuccessfulExplore Anywhere)
  Two -> do
    whenReturnTo do
      gather Set.ReturnToHeartOfTheElders
      gather Set.ReturnToKnYan
    gather Set.KnYan
    gather Set.HeartOfTheElders
    gather Set.AgentsOfYig
    gather Set.YigsVenom `orWhenReturnTo` gather Set.VenomousHate
    gather Set.ForgottenRuins
    gather Set.DeadlyTraps
    gather Set.Poison

    theJungleWatches <- recordedCardCodes <$> getRecordSet TheJungleWatches
    let theJungleWatchesCardDefs = mapMaybe lookupCardDef theJungleWatches

    when (notNull theJungleWatchesCardDefs) do
      placeInVictory theJungleWatchesCardDefs

    removeEvery [Locations.mouthOfKnYanTheCavernsMaw]
    startAt =<< place Locations.mouthOfKnYanTheDepthsBeneath

    setAsidePoisonedCount <- getSetAsidePoisonedCount
    setAside $ Locations.descentToYoth : replicate setAsidePoisonedCount Treacheries.poisoned

    isReturnTo <- getIsReturnTo

    let
      treacheries =
        guard (not isReturnTo)
          *> [ Treacheries.pitfall
             , Treacheries.noTurningBack
             , Treacheries.deepDark
             , Treacheries.finalMistake
             ]

    triangle <- Locations.darkHollow `orSampleIfReturnTo` [Locations.ruinsOfKnYan]
    square <- Locations.hallOfIdolatry `orSampleIfReturnTo` [Locations.chthonianDepths]
    diamond <- Locations.perilousGulch `orSampleIfReturnTo` [Locations.subterraneanSwamp]
    moon <- Locations.crystalPillars `orSampleIfReturnTo` [Locations.treacherousDescent]

    addExtraDeck ExplorationDeck
      =<< shuffle
        ( [ Locations.vastPassages
          , square
          , triangle
          , diamond
          , moon
          ]
            <> treacheries
        )

    setActDeck [Acts.cavernOfTheForgottenAge, Acts.descentIntoDark]
    setAgendaDeck [Agendas.theLonelyCaverns, Agendas.eyesInTheDark]

    setLayout part2Locations

    whenReturnTo do
      setAside [Enemies.harbingerOfValusiaTheSleeperReturns]
      addAdditionalReferences ["53045b"]
      createAbilityEffect EffectGameWindow
        $ mkAbility (SourceableWithCardCode (CardCode "53045b") ScenarioSource) 1
        $ forced
        $ Explored #after Anyone Anywhere (SuccessfulExplore Anywhere)

runAMessage :: Message -> HeartOfTheElders -> QueueT Message GameT HeartOfTheElders
runAMessage msg s@(HeartOfTheElders (attrs `With` metadata)) = case msg of
  StandaloneSetup -> do
    lead <- getLead
    setChaosTokens standaloneChaosTokens

    chooseOneM lead do
      questionLabeled
        "The investigators may choose how many paths are known to you (choose a number between 0 and 5). The more paths are known to you, the quicker and easier the scenario will be."
      for_ [0 .. 5] \n -> do
        labeled (tshow n) $ recordCount PathsAreKnownToYou n
    pure s
  PreScenarioSetup -> do
    story intro1

    lead <- getLead
    chooseOneM lead do
      withOwner Assets.ichtacaTheForgottenGuardian \iid -> do
        labeled "Let’s consult with Ichtaca." do
          story intro2
          putCampaignCardIntoPlay iid Assets.ichtacaTheForgottenGuardian

      withOwner Assets.alejandroVela \iid -> do
        labeled "Let’s consult with Alejandro." do
          story intro3
          putCampaignCardIntoPlay iid Assets.alejandroVela

      withOwner Assets.expeditionJournal \iid -> do
        labeled "Let’s consult the expedition journal." do
          story intro4
          putCampaignCardIntoPlay iid Assets.expeditionJournal
      labeled "I wish we knew more about this..." nothing
    pure s
  ScenarioResolution r -> case r of
    NoResolution -> do
      story noResolutionA
      pathsKnown <- getRecordCount PathsAreKnownToYou
      pillarTokens <- selectCountTokens Pillar (locationIs Locations.mouthOfKnYanTheCavernsMaw)
      when (pillarTokens > pathsKnown) do
        recordCount PathsAreKnownToYou pillarTokens
      push RestartScenario
      actStep <- getCurrentActStep
      pure $ HeartOfTheElders (attrs `With` metadata {reachedAct2 = reachedAct2 metadata || actStep >= 2})
    Resolution 1 -> do
      story resolution1A
      vengeanceCards <-
        filter (isJust . cdVengeancePoints . toCardDef)
          <$> scenarioField ScenarioVictoryDisplay
      recordSetInsert TheJungleWatches (map toCardCode vengeanceCards)
      allGainXp attrs
      push RestartScenario
      pure $ HeartOfTheElders (attrs `With` metadata {scenarioStep = Two})
    _ -> pure s
  _ -> HeartOfTheElders . (`with` metadata) <$> liftRunMessage msg attrs

runBMessage :: Message -> HeartOfTheElders -> QueueT Message GameT HeartOfTheElders
runBMessage msg s@(HeartOfTheElders (attrs `With` metadata)) = case msg of
  ScenarioResolution r -> do
    case r of
      NoResolution -> do
        story noResolutionB
        rescuedAlejandro <- getHasRecord TheInvestigatorsRescuedAlejandro
        push $ if rescuedAlejandro then R1 else R2
      Resolution n -> do
        story $ case n of
          1 -> resolution1B
          2 -> resolution2B
          _ -> error "invalid resolution"

        vengeance <- getTotalVengeanceInVictoryDisplay
        yigsFury <- getRecordCount YigsFury
        recordCount YigsFury (yigsFury + vengeance)

        inVictory <- selectAny $ VictoryDisplayCardMatch $ basic $ cardIs Enemies.harbingerOfValusia
        if inVictory
          then crossOut TheHarbingerIsStillAlive
          else do
            damage <-
              selectOne (enemyIs Enemies.harbingerOfValusia) >>= \case
                Just eid -> field EnemyDamage eid
                Nothing -> getRecordCount TheHarbingerIsStillAlive
            recordCount TheHarbingerIsStillAlive damage
        allGainXp attrs
        endOfScenario
    pure s
  _ -> HeartOfTheElders . (`with` metadata) <$> liftRunMessage msg attrs

instance RunMessage HeartOfTheElders where
  runMessage msg s@(HeartOfTheElders (attrs `With` metadata)) = runQueueT $ case msg of
    Setup ->
      runScenarioSetup (HeartOfTheElders . (`with` metadata)) attrs
        $ setupHeartOfTheElders metadata attrs
    Explore iid _ _ -> do
      checkWhen $ Window.AttemptExplore iid
      push $ Do msg
      pure s
    Do (Explore iid source locationMatcher) -> do
      explore iid source locationMatcher PlaceExplored 1
      pure s
    ResolveChaosToken _ Tablet iid -> do
      whenPoisoned iid failSkillTest
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist -> withLocationOf iid \lid -> placeDoom Cultist lid 1
        Tablet | isHardExpert attrs -> unlessPoisoned iid do
          poison <- getSetAsidePoisoned
          push $ CreateWeaknessInThreatArea poison iid
        ElderThing -> assignHorror iid ElderThing 1
        _ -> pure ()
      pure s
    UseCardAbility _ ScenarioSource 1 _ _ -> do
      getEncounterDeck >>= \case
        Deck [] -> pure ()
        Deck (x : _) -> shuffleCardsIntoDeck ExplorationDeck [x]
      pure s
    _ -> case scenarioStep metadata of
      One -> runAMessage msg s
      Two -> runBMessage msg s
