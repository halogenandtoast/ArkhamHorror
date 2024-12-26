module Arkham.Scenario.Scenarios.LostInTimeAndSpace (LostInTimeAndSpace (..), lostInTimeAndSpace) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence qualified as AS
import Arkham.Act.Types (Field (..))
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers hiding (defeated)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.LostInTimeAndSpace.FlavorText
import Arkham.Scenarios.LostInTimeAndSpace.Helpers
import Arkham.Trait hiding (Cultist, ElderThing)

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
      pure
        $ ChaosTokenValue Skull
        $ NegativeModifier
        $ if isEasyStandard attrs then min extradimensionalCount 5 else extradimensionalCount
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 5
    ElderThing -> do
      shroud <- maybe (pure 0) (fieldWithDefault 0 LocationShroud) =<< field InvestigatorLocation iid
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

readInvestigatorDefeat :: ReverseQueue m => ScenarioAttrs -> m ()
readInvestigatorDefeat a = do
  defeated <- select DefeatedInvestigator
  unless (null defeated) do
    storyOnly defeated investigatorDefeat
    for_ defeated (kill a)

instance RunMessage LostInTimeAndSpace where
  runMessage msg s@(LostInTimeAndSpace attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story intro
      pure s
    StandaloneSetup -> do
      setChaosTokens standaloneChaosTokens
      pure s
    Setup -> runScenarioSetup LostInTimeAndSpace attrs do
      gather Set.LostInTimeAndSpace
      gather Set.Sorcery
      gather Set.TheBeyond
      gather Set.HideousAbominations
      gather Set.AgentsOfYogSothoth

      startAt =<< place Locations.anotherDimension

      setAside
        [ Locations.theEdgeOfTheUniverse
        , Locations.tearThroughTime
        , Enemies.yogSothoth
        ]
      setActDeck
        [ Acts.outOfThisWorld
        , Acts.intoTheBeyond
        , Acts.closeTheRift
        , Acts.findingANewWay
        ]
      setAgendaDeck
        [ Agendas.allIsOne
        , Agendas.pastPresentAndFuture
        , Agendas.breakingThrough
        , Agendas.theEndOfAllThings
        ]
    After (PassedSkillTest iid _ _ (ChaosTokenTarget token) _ _) -> do
      case (isHardExpert attrs, chaosTokenFace token) of
        (True, Cultist) -> discardUntilFirst iid Cultist Deck.EncounterDeck (basic #location)
        (_, Tablet) -> do
          mYogSothothId <- selectOne (EnemyWithTitle "Yog-Sothoth")
          for_ mYogSothothId $ \eid -> initiateEnemyAttack eid attrs iid
        _ -> pure ()
      pure s
    After (FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _) -> do
      case token.face of
        Cultist -> discardUntilFirst iid Cultist Deck.EncounterDeck (basic #location)
        Tablet -> do
          mYogSothothId <- selectOne (EnemyWithTitle "Yog-Sothoth")
          for_ mYogSothothId $ \eid -> initiateEnemyAttack eid attrs iid
        _ -> pure ()
      pure s
    ResolveChaosToken _ Cultist iid -> do
      drawAnotherChaosToken iid
      pure s
    RequestedEncounterCard (isSource attrs -> True) (Just iid) (Just card) -> do
      moveTo attrs iid =<< placeLocation (EncounterCard card)
      pure s
    ScenarioResolution NoResolution -> do
      step <- fieldMap ActSequence (AS.unActStep . AS.actStep) =<< selectJust AnyAct
      push $ if step == 4 then R2 else R4
      pure s
    ScenarioResolution (Resolution 1) -> do
      readInvestigatorDefeat attrs
      story resolution1
      record TheInvestigatorsClosedTheTearInReality
      eachInvestigator \iid -> sufferTrauma iid 2 2
      allGainXpWithBonus attrs $ toBonus "resolution1" 5
      endOfScenario
      pure s
    ScenarioResolution (Resolution 2) -> do
      readInvestigatorDefeat attrs
      story resolution2
      endOfScenario
      pure s
    ScenarioResolution (Resolution 3) -> do
      readInvestigatorDefeat attrs
      story resolution3
      record YogSothothHasFledToAnotherDimension
      eachInvestigator (kill attrs)
      endOfScenario
      pure s
    ScenarioResolution (Resolution 4) -> do
      readInvestigatorDefeat attrs
      story resolution4
      record YogSothothToreApartTheBarrierBetweenWorldsAndBecameOneWithAllReality
      eachInvestigator drivenInsane
      endOfScenario
      pure s
    _ -> LostInTimeAndSpace <$> liftRunMessage msg attrs
