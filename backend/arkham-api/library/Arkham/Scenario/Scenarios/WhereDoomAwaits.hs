module Arkham.Scenario.Scenarios.WhereDoomAwaits (WhereDoomAwaits (..), whereDoomAwaits) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Agenda.Sequence qualified as AS
import Arkham.Agenda.Types (Field (..))
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers hiding (skillTestModifier)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.WhereDoomAwaits.Story
import Arkham.Trait hiding (Cultist, ElderThing, Expert)

newtype WhereDoomAwaits = WhereDoomAwaits ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whereDoomAwaits :: Difficulty -> WhereDoomAwaits
whereDoomAwaits difficulty =
  scenario
    WhereDoomAwaits
    "02274"
    "Where Doom Awaits"
    difficulty
    [ "divergingPath1 divergingPath2 divergingPath3"
    , "baseOfTheHill ascendingPath sentinelPeak"
    , "alteredPath1 alteredPath2 alteredPath3"
    ]

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
  , Cultist
  , Tablet
  , ElderThing
  , AutoFail
  , ElderSign
  ]

standaloneCampaignLog :: CampaignLog
standaloneCampaignLog =
  mkCampaignLog
    { campaignLogRecorded = setFromList [NoBroodEscapedIntoTheWild]
    }

instance HasChaosTokenValue WhereDoomAwaits where
  getChaosTokenValue iid chaosTokenFace (WhereDoomAwaits attrs) = case chaosTokenFace of
    Skull -> do
      isAltered <- selectAny $ locationWithInvestigator iid <> LocationWithTrait Altered
      if isAltered
        then pure $ toChaosTokenValue attrs Skull 3 5
        else pure $ toChaosTokenValue attrs Skull 1 2
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> do
      agendaId <- selectJust AnyAgenda
      agendaStep <- fieldMap AgendaSequence (AS.unAgendaStep . AS.agendaStep) agendaId
      pure
        $ ChaosTokenValue Tablet
        $ if isEasyStandard attrs
          then NegativeModifier (if agendaStep == 2 then 4 else 2)
          else if agendaStep == 2 then AutoFailModifier else NegativeModifier 3
    ElderThing -> pure $ ChaosTokenValue ElderThing (NegativeModifier 0) -- determined by an effect
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage WhereDoomAwaits where
  runMessage msg s@(WhereDoomAwaits attrs) = runQueueT $ case msg of
    PreScenarioSetup -> do
      story intro
      whenHasRecord NaomiHasTheInvestigatorsBacks $ story introPart1
      pure s
    StandaloneSetup -> do
      setChaosTokens standaloneChaosTokens
      pure . WhereDoomAwaits $ attrs & standaloneCampaignLogL .~ standaloneCampaignLog
    Setup -> runScenarioSetup WhereDoomAwaits attrs do
      gather Set.WhereDoomAwaits
      gather Set.BeastThralls
      gather Set.Sorcery
      gather Set.BishopsThralls
      gather Set.StrikingFear
      gather Set.AncientEvils
      gather Set.ChillingCold

      noBroodEscaped <- getHasRecord NoBroodEscapedIntoTheWild
      broodEscapedCount <- if noBroodEscaped then pure 0 else getRecordCount BroodEscapedIntoTheWild
      silasBishopPutOutOfMisery <- getHasRecord TheInvestigatorsPutSilasBishopOutOfHisMisery

      startAt =<< place Locations.baseOfTheHill
      ascendingPath <- place Locations.ascendingPath
      place_ Locations.sentinelPeak

      when silasBishopPutOutOfMisery do
        (conglomerationOfSpheres, rest) <- splitAt 1 <$> gatherEncounterSet Set.HideousAbominations
        for_ conglomerationOfSpheres (`createEnemyAt_` ascendingPath)
        shuffleCardsIntoDeck Deck.EncounterDeck rest

      divergingPaths <-
        sampleN 3
          $ Locations.slaughteredWoods
          :| [ Locations.eerieGlade
             , Locations.destroyedPath
             , Locations.frozenSpring
             ]

      alteredPaths <-
        sampleN 3
          $ Locations.dimensionalGap
          :| [ Locations.aTearInThePath
             , Locations.uprootedWoods
             , Locations.lostMemories
             ]

      addChaosToken $ case attrs.difficulty of
        Easy -> MinusThree
        Standard -> MinusFive
        Hard -> MinusSix
        Expert -> MinusSeven

      when (broodEscapedCount > 0) $ placeDoomOnAgenda broodEscapedCount

      lead <- getLead
      whenHasRecord NaomiHasTheInvestigatorsBacks $ push $ GainClues lead (toSource attrs) 1

      setAside $ Enemies.sethBishop : divergingPaths <> alteredPaths
      setAgendaDeck [Agendas.callingForthTheOldOnes, Agendas.beckoningForPower]

      useV1 <- getHasRecord TheInvestigatorsRestoredSilasBishop
      useV2 <-
        liftA2
          (||)
          (getHasRecord TheInvestigatorsFailedToRecoverTheNecronomicon)
          (getHasRecord TheNecronomiconWasStolen)
      let
        ascendingTheHill = case (useV1, useV2) of
          (True, _) -> Acts.ascendingTheHillV1
          (False, True) -> Acts.ascendingTheHillV2
          (False, False) -> Acts.ascendingTheHillV3
      setActDeck [Acts.thePathToTheHill, ascendingTheHill, Acts.theGateOpens]
    ResolveChaosToken drawnToken Cultist iid -> do
      withSkillTest \sid -> do
        skillTestModifier sid drawnToken.face sid CancelSkills
        push CancelSkillEffects
        drawAnotherChaosToken iid
      pure s
    ResolveChaosToken drawnToken ElderThing iid -> do
      push
        $ DiscardTopOfDeck
          iid
          (if isEasyStandard attrs then 2 else 3)
          (ChaosTokenEffectSource ElderThing)
          (Just $ ChaosTokenTarget drawnToken)
      pure s
    DiscardedTopOfDeck _iid cards _ target@(ChaosTokenTarget (chaosTokenFace -> ElderThing)) -> do
      let n = sum $ map (toPrintedCost . fromMaybe (StaticCost 0) . cdCost . toCardDef) cards
      withSkillTest \sid -> push $ CreateChaosTokenValueEffect sid (-n) (toSource attrs) target
      pure s
    ScenarioResolution NoResolution -> do
      push R2
      pure s
    ScenarioResolution (Resolution 1) -> do
      story resolution1
      record TheInvestigatorsEnteredTheGate
      allGainXp attrs
      endOfScenario
      pure s
    ScenarioResolution (Resolution 2) -> do
      story resolution2
      record YogSothothToreApartTheBarrierBetweenWorldsAndBecameOneWithAllReality
      eachInvestigator drivenInsane
      gameOver
      pure s
    PlacedLocation name _ lid -> do
      when (name == "Altered Path") $ do
        alteredCount <- selectCount $ LocationWithTrait Altered
        push (SetLocationLabel lid $ "alteredPath" <> tshow alteredCount)
      when (name == "Diverging Path") $ do
        woodsCount <- selectCount $ LocationWithTrait Woods
        push (SetLocationLabel lid $ "divergingPath" <> tshow woodsCount)
      pure s
    _ -> WhereDoomAwaits <$> liftRunMessage msg attrs
