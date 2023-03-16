module Arkham.Scenario.Scenarios.WhereDoomAwaits
  ( WhereDoomAwaits(..)
  , whereDoomAwaits
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Agenda.Sequence qualified as AS
import Arkham.Agenda.Types ( Field (..) )
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Card.Cost
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Difficulty
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding ( RevealLocation )
import Arkham.Message
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.WhereDoomAwaits.Story
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Trait hiding ( Cultist, Expert )
import Data.Maybe ( fromJust )

newtype WhereDoomAwaits = WhereDoomAwaits ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whereDoomAwaits :: Difficulty -> WhereDoomAwaits
whereDoomAwaits difficulty = scenario
  WhereDoomAwaits
  "02274"
  "Where Doom Awaits"
  difficulty
  [ "divergingPath1 divergingPath2 divergingPath3"
  , "baseOfTheHill ascendingPath sentinelPeak"
  , "alteredPath1 alteredPath2 alteredPath3"
  ]

standaloneTokens :: [TokenFace]
standaloneTokens =
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
standaloneCampaignLog = mkCampaignLog
  { campaignLogRecorded = setFromList [NoBroodEscapedIntoTheWild]
  }

instance HasTokenValue WhereDoomAwaits where
  getTokenValue iid tokenFace (WhereDoomAwaits attrs) = case tokenFace of
    Skull -> do
      isAltered <-
        selectAny
        $ LocationWithInvestigator (InvestigatorWithId iid)
        <> LocationWithTrait Altered
      if isAltered
        then pure $ toTokenValue attrs Skull 3 5
        else pure $ toTokenValue attrs Skull 1 2
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> do
      agendaId <- selectJust AnyAgenda
      agendaStep <- fieldMap
        AgendaSequence
        (AS.unAgendaStep . AS.agendaStep)
        agendaId
      pure $ TokenValue
        Tablet
        (if isEasyStandard attrs
          then NegativeModifier (if agendaStep == 2 then 4 else 2)
          else if agendaStep == 2 then AutoFailModifier else NegativeModifier 3
        )
    ElderThing -> pure $ TokenValue ElderThing (NegativeModifier 0) -- determined by an effect
    otherFace -> getTokenValue iid otherFace attrs

instance RunMessage WhereDoomAwaits where
  runMessage msg s@(WhereDoomAwaits attrs) = case msg of
    SetTokensForScenario -> do
      standalone <- getIsStandalone
      s <$ if standalone then push (SetTokens standaloneTokens) else pure ()
    StandaloneSetup -> do
      pure
        . WhereDoomAwaits
        $ attrs
        & standaloneCampaignLogL
        .~ standaloneCampaignLog
    Setup -> do
      investigatorIds <- allInvestigatorIds
      lead <- getLead
      encounterDeck <- buildEncounterDeckExcluding
        [Enemies.sethBishop]
        [ EncounterSet.WhereDoomAwaits
        , EncounterSet.Whippoorwills
        , EncounterSet.BeastThralls
        , EncounterSet.Dunwich
        , EncounterSet.Sorcery
        , EncounterSet.BishopsThralls
        , EncounterSet.StrikingFear
        , EncounterSet.AncientEvils
        , EncounterSet.ChillingCold
        ]

      useV1 <- getHasRecord TheInvestigatorsRestoredSilasBishop
      useV2 <- liftA2
        (||)
        (getHasRecord TheInvestigatorsFailedToRecoverTheNecronomicon)
        (getHasRecord TheNecronomiconWasStolen)

      let
        ascendingTheHill = case (useV1, useV2) of
          (True, _) -> Acts.ascendingTheHillV1
          (False, True) -> Acts.ascendingTheHillV2
          (False, False) -> Acts.ascendingTheHillV3

      naomiHasTheInvestigatorsBacks <- getHasRecord
        NaomiHasTheInvestigatorsBacks
      noBroodEscaped <- getHasRecord NoBroodEscapedIntoTheWild
      broodEscapedCount <- if noBroodEscaped
        then pure 0
        else getRecordCount BroodEscapedIntoTheWild
      silasBishopPutOutOfMisery <- getHasRecord
        TheInvestigatorsPutSilasBishopOutOfHisMisery

      (baseOfTheHillId, placeBaseOfTheHill) <- placeLocationCard
        Locations.baseOfTheHill
      (ascendingPathId, placeAscendingPath) <- placeLocationCard
        Locations.ascendingPath
      placeSentinelPeak <- placeLocationCard_ Locations.sentinelPeak

      silasMsgs <- if silasBishopPutOutOfMisery
        then do
          result <- gatherEncounterSet EncounterSet.HideousAbominations
          let
            conglomerationOfSpheres = fromJust . headMay $ result
            rest = drop 1 result
          pure
            [ SpawnEnemyAt
              (EncounterCard conglomerationOfSpheres)
              ascendingPathId
            , ShuffleCardsIntoDeck Deck.EncounterDeck $ map EncounterCard rest
            ]
        else pure []

      divergingPaths <- genCards . take 3 =<< shuffleM
        [ Locations.slaughteredWoods
        , Locations.eerieGlade
        , Locations.destroyedPath
        , Locations.frozenSpring
        ]

      alteredPaths <- genCards . take 3 =<< shuffleM
        [ Locations.dimensionalGap
        , Locations.aTearInThePath
        , Locations.uprootedWoods
        , Locations.lostMemories
        ]

      let
        token = case scenarioDifficulty attrs of
          Easy -> MinusThree
          Standard -> MinusFive
          Hard -> MinusSix
          Expert -> MinusSeven

      pushAll
        $ story investigatorIds intro
        : [ story investigatorIds introPart1 | naomiHasTheInvestigatorsBacks ]
        <> [ GainClues lead 1 | naomiHasTheInvestigatorsBacks ]
        <> [ AddToken token
           , SetEncounterDeck encounterDeck
           , SetAgendaDeck
           , SetActDeck
           ]
        <> replicate broodEscapedCount PlaceDoomOnAgenda
        <> [placeBaseOfTheHill, placeAscendingPath, placeSentinelPeak]
        <> silasMsgs
        <> [ RevealLocation Nothing baseOfTheHillId
           , MoveAllTo (toSource attrs) baseOfTheHillId
           ]

      setAsideCards <- genCards [Enemies.silasBishop]

      agendas <- genCards
        [Agendas.callingForthTheOldOnes, Agendas.beckoningForPower]
      acts <- genCards
        [Acts.thePathToTheHill, ascendingTheHill, Acts.theGateOpens]

      WhereDoomAwaits <$> runMessage
        msg
        (attrs
        & (actStackL . at 1 ?~ acts)
        & (agendaStackL . at 1 ?~ agendas)
        & (setAsideCardsL
          <>~ (divergingPaths <> alteredPaths <> setAsideCards)
          )
        )
    ResolveToken drawnToken Cultist iid -> s <$ pushAll
      [ CreateWindowModifierEffect
        EffectSkillTestWindow
        (EffectModifiers $ toModifiers attrs [CancelSkills])
        (TokenSource drawnToken)
        SkillTestTarget
      , CancelSkillEffects
      , DrawAnotherToken iid
      ]
    ResolveToken drawnToken ElderThing iid -> s <$ push
      (DiscardTopOfDeck
        iid
        (if isEasyStandard attrs then 2 else 3)
        (TokenEffectSource ElderThing)
        (Just $ TokenTarget drawnToken)
      )
    DiscardedTopOfDeck _iid cards _ target@(TokenTarget token) ->
      s <$ case tokenFace token of
        ElderThing -> do
          let
            n = sum $ map
              (toPrintedCost . fromMaybe (StaticCost 0) . cdCost . toCardDef)
              cards
          push $ CreateTokenValueEffect (-n) (toSource attrs) target
        _ -> pure ()
    ScenarioResolution NoResolution ->
      s <$ push (ScenarioResolution $ Resolution 2)
    ScenarioResolution (Resolution 1) -> do
      xp <- getXp
      investigatorIds <- allInvestigatorIds
      pushAll
        $ [ story investigatorIds resolution1
          , Record TheInvestigatorsEnteredTheGate
          ]
        <> [ GainXP iid n | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
      pure s
    ScenarioResolution (Resolution 2) -> do
      investigatorIds <- allInvestigatorIds
      pushAll
        $ [ story investigatorIds resolution2
          , Record
            YogSothothToreApartTheBarrierBetweenWorldsAndBecameOneWithAllReality
          ]
        <> [ DrivenInsane iid | iid <- investigatorIds ]
        <> [GameOver]
      pure s
    PlacedLocation name _ lid -> do
      when (name == "Altered Path") $ do
        alteredCount <- selectCount $ LocationWithTrait Altered
        push (SetLocationLabel lid $ "alteredPath" <> tshow alteredCount)
      when (name == "Diverging Path") $ do
        woodsCount <- selectCount $ LocationWithTrait Woods
        push (SetLocationLabel lid $ "divergingPath" <> tshow woodsCount)
      pure s
    _ -> WhereDoomAwaits <$> runMessage msg attrs
