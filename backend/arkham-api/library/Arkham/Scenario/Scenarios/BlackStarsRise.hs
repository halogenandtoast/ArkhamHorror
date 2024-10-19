module Arkham.Scenario.Scenarios.BlackStarsRise (BlackStarsRise (..), blackStarsRise) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Agenda.Types (Field (..))
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Card
import Arkham.ChaosToken
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message (CanAdvance (..))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers hiding (recordSetInsert)
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.BlackStarsRise.Story
import Arkham.SkillTest
import Arkham.Trait qualified as Trait

newtype BlackStarsRise = BlackStarsRise ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackStarsRise :: Difficulty -> BlackStarsRise
blackStarsRise difficulty =
  scenarioWith
    BlackStarsRise
    "03274"
    "Black Stars Rise"
    difficulty
    [ ".                cloister      .           northTower      ."
    , "knightsHall      cloister      .           northTower      ."
    , "knightsHall      abbeyChurch    brokenSteps .               outerWall"
    , "chapelOfStAubert abbeyChurch    brokenSteps .               outerWall"
    , "chapelOfStAubert chœurGothique  .           grandRue        ."
    , ".                chœurGothique  .           grandRue        ."
    , ".                abbeyTower     .           porteDeLAvancée ."
    , ".                abbeyTower     .           porteDeLAvancée ."
    ]
    (decksLayoutL .~ ["act1 agenda1 agenda2 act2"])

instance HasChaosTokenValue BlackStarsRise where
  getChaosTokenValue iid chaosTokenFace (BlackStarsRise attrs) = case chaosTokenFace of
    Skull -> do
      maxDoom <- fieldMax AgendaDoom AnyAgenda
      totalDoom <- selectSum AgendaDoom AnyAgenda
      pure $ toChaosTokenValue attrs Skull maxDoom totalDoom
    Cultist ->
      if isEasyStandard attrs
        then do
          modifier <- do
            getSkillTestAction >>= \case
              Just action | action `elem` [#evade, #fight] -> do
                getSkillTestTarget >>= \case
                  Just (EnemyTarget eid) -> do
                    hasDoom <- fieldP EnemyDoom (> 0) eid
                    pure $ if hasDoom then AutoFailModifier else NoModifier
                  _ -> pure NoModifier
              _ -> pure NoModifier
          pure $ ChaosTokenValue Cultist modifier
        else do
          anyEnemyWithDoom <- selectAny EnemyWithAnyDoom
          let modifier = if anyEnemyWithDoom then AutoFailModifier else NoModifier
          pure $ ChaosTokenValue Cultist modifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 2 3
    otherFace -> getChaosTokenValue iid otherFace attrs

data Version = TheFloodBelow | TheVortexAbove
  deriving stock Eq

versions :: NonEmpty Version
versions = TheFloodBelow :| [TheVortexAbove]

standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
  [ PlusOne
  , Zero
  , Zero
  , MinusOne
  , MinusOne
  , MinusOne
  , MinusTwo
  , MinusTwo
  , MinusThree
  , MinusThree
  , MinusFour
  , MinusFive
  , Skull
  , Skull
  , Skull
  , AutoFail
  , ElderSign
  ]

instance RunMessage BlackStarsRise where
  runMessage msg s@(BlackStarsRise attrs) = runQueueT $ case msg of
    PreScenarioSetup -> do
      story intro
      whenInterviewed Assets.ashleighClarke $ story ashleighsInformation
      pure s
    StandaloneSetup -> do
      lead <- getLead
      theManInThePallidMask <- genCard Enemies.theManInThePallidMask
      randomToken <- sample $ Cultist :| [Tablet, ElderThing]
      setChaosTokens $ standaloneChaosTokens <> [randomToken, randomToken]
      shuffleCardsIntoDeck lead [theManInThePallidMask]
      pure s
    Setup -> runScenarioSetup BlackStarsRise attrs do
      gather Set.BlackStarsRise
      gather Set.EvilPortents
      gather Set.Byakhee
      gather Set.InhabitantsOfCarcosa
      gather Set.TheStranger
      gather Set.DarkCult
      gather Set.AncientEvils

      (agenda2a, agenda2c, abbeyTower, chapelOfStAubert) <-
        sample versions <&> \case
          TheVortexAbove ->
            ( Agendas.letTheStormRageTheVortexAbove
            , Agendas.theEntityAboveTheVortexAbove
            , Locations.abbeyTowerThePathIsOpen
            , Locations.chapelOfStAubertWatersForbidden
            )
          TheFloodBelow ->
            ( Agendas.letTheStormRageTheFloodBelow
            , Agendas.theEntityAboveTheFloodBelow
            , Locations.abbeyTowerSpiresForbidden
            , Locations.chapelOfStAubertThePathIsOpen
            )

      choeurGothique <- sample2 Locations.choeurGothique_292 Locations.choeurGothique_293

      setAside
        [ Enemies.tidalTerror
        , Enemies.tidalTerror
        , Enemies.riftSeeker
        , Enemies.riftSeeker
        , Acts.openThePathAbove
        , Acts.openThePathBelow
        , Enemies.beastOfAldebaran
        , Locations.cloister
        , Locations.knightsHall
        , abbeyTower
        , chapelOfStAubert
        , choeurGothique
        ]

      northTower <- sample2 Locations.northTower_287 Locations.northTower_288
      outerWall <- sample2 Locations.outerWall_285 Locations.outerWall_286
      brokenSteps <- sample2 Locations.brokenSteps_289 Locations.brokenSteps_290

      startAt =<< place Locations.porteDeLAvancee
      placeAll [northTower, outerWall, brokenSteps, Locations.grandRue, Locations.abbeyChurch]

      unlessStandalone do
        eachInvestigator \iid ->
          searchCollectionForRandom iid attrs
            $ BasicWeaknessCard
            <> mapOneOf CardWithTrait [Trait.Madness, Trait.Pact, Trait.Cultist, Trait.Detective]

      addChaosToken $ case attrs.difficulty of
        Easy -> MinusThree
        Standard -> MinusFive
        Hard -> MinusSix
        Expert -> MinusSeven

      setAgendaDeckN 1 [Agendas.theTideRises, agenda2a, Agendas.theCityFloods]
      setAgendaDeckN 2 [Agendas.theRitualBeginsBlackStarsRise, agenda2c, Agendas.swallowedSky]
    PlaceDoomOnAgenda n canAdvance -> do
      agendas <- select AnyAgenda
      lead <- getLead
      chooseTargetM lead agendas \agenda -> do
        placeDoom attrs agenda n
        pushWhen (canAdvance == CanAdvance) AdvanceAgendaIfThresholdSatisfied
      pure s
    PassedSkillTest _ _ _ (ChaosTokenTarget token) _ n | n < 1 -> do
      when (chaosTokenFace token == Tablet) $ do
        selectEach AnyAgenda \agenda -> placeDoom attrs agenda 1
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      when (token.face == Tablet) do
        selectEach AnyAgenda \agenda -> placeDoom attrs agenda 1
      when (token.face == ElderThing) do
        findAndDrawEncounterCard iid $ #enemy <> CardWithTrait Trait.Byakhee
      pure s
    ResolveChaosToken _ Cultist iid -> do
      drawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ Tablet iid -> do
      drawAnotherChaosToken iid
      pure s
    ScenarioResolution res -> do
      let
        updateSlain = selectForMaybeM (VictoryDisplayCardMatch $ basic $ cardIs Enemies.ashleighClarke) \ashleigh ->
          recordSetInsert VIPsSlain [toCardCode ashleigh]
      case res of
        NoResolution -> push R3
        Resolution 1 -> do
          story resolution1
          record YouOpenedThePathBelow
          removeAllChaosTokens Cultist
          removeAllChaosTokens Tablet
          removeAllChaosTokens ElderThing
          addChaosToken Cultist
          addChaosToken Cultist
          addChaosToken Tablet
          addChaosToken Tablet
          updateSlain
          allGainXp attrs
          endOfScenario
        Resolution 2 -> do
          story resolution2
          record YouOpenedThePathAbove
          removeAllChaosTokens Cultist
          removeAllChaosTokens Tablet
          removeAllChaosTokens ElderThing
          addChaosToken Cultist
          addChaosToken Cultist
          addChaosToken ElderThing
          addChaosToken ElderThing
          updateSlain
          allGainXp attrs
          endOfScenario
        Resolution 3 -> do
          story resolution3
          record TheRealmOfCarcosaMergedWithOurOwnAndHasturRulesOverThemBoth
          eachInvestigator drivenInsane
          gameOver
        _ -> error "Unknown resolution"
      pure s
    RequestedPlayerCard iid source mcard _ | isSource attrs source -> do
      for_ mcard $ push . AddCardToDeckForCampaign iid
      pure s
    _ -> BlackStarsRise <$> liftRunMessage msg attrs
