module Arkham.Scenario.Scenarios.LostInTimeAndSpace (
  LostInTimeAndSpace (..),
  lostInTimeAndSpace,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence qualified as AS
import Arkham.Act.Types (Field (..))
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Attack
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message
import Arkham.Movement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.LostInTimeAndSpace.FlavorText
import Arkham.Trait hiding (Cultist)

newtype LostInTimeAndSpace = LostInTimeAndSpace ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostInTimeAndSpace :: Difficulty -> LostInTimeAndSpace
lostInTimeAndSpace difficulty =
  scenario
    LostInTimeAndSpace
    "02311"
    "Lost in Time and Space"
    difficulty
    [ ".              .                  .                  tearThroughSpace2 tearThroughSpace2    tearThroughSpace1    tearThroughSpace1  .                 .                 ."
    , ".              .                  .                  tearThroughSpace2 tearThroughSpace2    tearThroughSpace1    tearThroughSpace1  .                 .                 ."
    , ".              tearThroughSpace3  tearThroughSpace3  .                 .                    .                    .                  tearThroughSpace4 tearThroughSpace4 ."
    , ".              tearThroughSpace3  tearThroughSpace3  .                 .                    .                    .                  tearThroughSpace4 tearThroughSpace4 ."
    , "endlessBridge2 endlessBridge2     endlessBridge1     endlessBridge1    .                    .                    prismaticCascade1  prismaticCascade1 prismaticCascade2 prismaticCascade2"
    , "endlessBridge2 endlessBridge2     endlessBridge1     endlessBridge1    .                    .                    prismaticCascade1  prismaticCascade1 prismaticCascade2 prismaticCascade2"
    , ".              dimensionalDoorway dimensionalDoorway .                 anotherDimension     anotherDimension     .                  stepsOfYhagharl   stepsOfYhagharl   ."
    , ".              dimensionalDoorway dimensionalDoorway .                 anotherDimension     anotherDimension     .                  stepsOfYhagharl   stepsOfYhagharl   ."
    , ".              .                  .                  .                 tearThroughTime      tearThroughTime      .                  .                 .                 ."
    , ".              .                  .                  .                 tearThroughTime      tearThroughTime      .                  .                 .                 ."
    , ".              .                  .                  .                 theEdgeOfTheUniverse theEdgeOfTheUniverse .                  .                 .                 ."
    , ".              .                  .                  .                 theEdgeOfTheUniverse theEdgeOfTheUniverse .                  .                 .                 ."
    ]

instance HasChaosTokenValue LostInTimeAndSpace where
  getChaosTokenValue iid chaosTokenFace (LostInTimeAndSpace attrs) = case chaosTokenFace of
    Skull -> do
      extradimensionalCount <- selectCount $ LocationWithTrait Extradimensional
      pure $
        ChaosTokenValue
          Skull
          ( NegativeModifier $
              if isEasyStandard attrs
                then min extradimensionalCount 5
                else extradimensionalCount
          )
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 5
    ElderThing -> do
      mlid <- field InvestigatorLocation iid
      shroud <- maybe (pure 0) (field LocationShroud) mlid
      pure $ toChaosTokenValue attrs ElderThing shroud (shroud * 2)
    otherFace -> getChaosTokenValue iid otherFace attrs

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

readInvestigatorDefeat :: (HasGame m) => ScenarioAttrs -> m [Message]
readInvestigatorDefeat a = do
  defeatedInvestigatorIds <- selectList DefeatedInvestigator
  if null defeatedInvestigatorIds
    then pure []
    else
      pure $
        [story defeatedInvestigatorIds investigatorDefeat]
          <> [ InvestigatorKilled (toSource a) iid
             | iid <- defeatedInvestigatorIds
             ]

instance RunMessage LostInTimeAndSpace where
  runMessage msg s@(LostInTimeAndSpace attrs) = case msg of
    SetChaosTokensForScenario -> do
      whenStandalone $ push (SetChaosTokens standaloneChaosTokens)
      pure s
    Setup -> do
      investigatorIds <- allInvestigatorIds
      encounterDeck <-
        buildEncounterDeckExcluding
          [Enemies.yogSothoth]
          [ EncounterSet.LostInTimeAndSpace
          , EncounterSet.Sorcery
          , EncounterSet.TheBeyond
          , EncounterSet.HideousAbominations
          , EncounterSet.AgentsOfYogSothoth
          ]

      (anotherDimensionId, placeAnotherDimension) <-
        placeLocationCard
          Locations.anotherDimension

      pushAll
        [ story investigatorIds intro
        , SetEncounterDeck encounterDeck
        , SetAgendaDeck
        , SetActDeck
        , placeAnotherDimension
        , RevealLocation Nothing anotherDimensionId
        , MoveAllTo (toSource attrs) anotherDimensionId
        ]

      setAsideCards <-
        genCards
          [ Locations.theEdgeOfTheUniverse
          , Locations.tearThroughTime
          , Enemies.yogSothoth
          ]
      acts <-
        genCards
          [ Acts.outOfThisWorld
          , Acts.intoTheBeyond
          , Acts.closeTheRift
          , Acts.findingANewWay
          ]
      agendas <-
        genCards
          [ Agendas.allIsOne
          , Agendas.pastPresentAndFuture
          , Agendas.breakingThrough
          , Agendas.theEndOfAllThings
          ]

      LostInTimeAndSpace
        <$> runMessage
          msg
          ( attrs
              & (setAsideCardsL .~ setAsideCards)
              & (actStackL . at 1 ?~ acts)
              & (agendaStackL . at 1 ?~ agendas)
          )
    After (PassedSkillTest iid _ _ (ChaosTokenTarget token) _ _) -> do
      case (isHardExpert attrs, chaosTokenFace token) of
        (True, Cultist) ->
          push
            ( DiscardUntilFirst
                iid
                (ChaosTokenEffectSource Cultist)
                Deck.EncounterDeck
                (BasicCardMatch $ CardWithType LocationType)
            )
        (_, Tablet) -> do
          mYogSothothId <- selectOne (EnemyWithTitle "Yog-Sothoth")
          for_ mYogSothothId $ \eid -> push (EnemyAttack $ enemyAttack eid attrs iid)
        _ -> pure ()
      pure s
    After (FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _) -> do
      case chaosTokenFace token of
        Cultist ->
          push $
            DiscardUntilFirst
              iid
              (ChaosTokenEffectSource Cultist)
              Deck.EncounterDeck
              (BasicCardMatch $ CardWithType LocationType)
        Tablet -> do
          mYogSothothId <- selectOne (EnemyWithTitle "Yog-Sothoth")
          for_ mYogSothothId $ \eid -> push (EnemyAttack $ enemyAttack eid attrs iid)
        _ -> pure ()
      pure s
    RequestedEncounterCard (isSource attrs -> True) (Just iid) (Just card) ->
      do
        (locationId, placement) <- placeLocation (EncounterCard card)
        pushAll [placement, MoveTo $ move attrs iid locationId]
        pure s
    ScenarioResolution NoResolution -> do
      actId <- selectJust AnyAct
      step <- fieldMap ActSequence (AS.unActStep . AS.actStep) actId
      push (ScenarioResolution . Resolution $ if step == 4 then 2 else 4)
      pure . LostInTimeAndSpace $ attrs & inResolutionL .~ True
    ScenarioResolution (Resolution 1) -> do
      msgs <- readInvestigatorDefeat attrs
      investigatorIds <- allInvestigatorIds
      xp <- getXp
      pushAll $
        msgs
          <> [ story investigatorIds resolution1
             , Record TheInvestigatorsClosedTheTearInReality
             ]
          <> [SufferTrauma iid 2 2 | iid <- investigatorIds]
          <> [GainXP iid (toSource attrs) (n + 5) | (iid, n) <- xp]
          <> [EndOfGame Nothing]
      pure . LostInTimeAndSpace $ attrs & inResolutionL .~ True
    ScenarioResolution (Resolution 2) -> do
      msgs <- readInvestigatorDefeat attrs
      investigatorIds <- allInvestigatorIds
      pushAll $ msgs <> [story investigatorIds resolution2, EndOfGame Nothing]

      pure . LostInTimeAndSpace $ attrs & inResolutionL .~ True
    ScenarioResolution (Resolution 3) -> do
      msgs <- readInvestigatorDefeat attrs
      investigatorIds <- allInvestigatorIds
      pushAll $
        msgs
          <> [ story investigatorIds resolution3
             , Record YogSothothHasFledToAnotherDimension
             ]
          <> [InvestigatorKilled (toSource attrs) iid | iid <- investigatorIds]
          <> [EndOfGame Nothing]
      pure . LostInTimeAndSpace $ attrs & inResolutionL .~ True
    ScenarioResolution (Resolution 4) -> do
      msgs <- readInvestigatorDefeat attrs
      investigatorIds <- allInvestigatorIds
      pushAll $
        msgs
          <> [ story investigatorIds resolution4
             , Record
                YogSothothToreApartTheBarrierBetweenWorldsAndBecameOneWithAllReality
             ]
          <> [DrivenInsane iid | iid <- investigatorIds]
          <> [EndOfGame Nothing]
      pure . LostInTimeAndSpace $ attrs & inResolutionL .~ True
    _ -> LostInTimeAndSpace <$> runMessage msg attrs
