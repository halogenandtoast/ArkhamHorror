module Arkham.Scenario.Scenarios.AThousandShapesOfHorror (
  AThousandShapesOfHorror (..),
  aThousandShapesOfHorror,
) where

import Arkham.Prelude hiding ((.=))

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Attack
import Arkham.CampaignLogKey
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted hiding (setActDeck, setAgendaDeck)
import Arkham.Projection
import Arkham.Scenario.Runner hiding (placeLocationCard, pushAll, story)
import Arkham.Scenario.Runner qualified as Msg
import Arkham.Scenario.Setup
import Arkham.Trait (Trait (Graveyard))
import Arkham.Treachery.Cards qualified as Treacheries

newtype AThousandShapesOfHorror = AThousandShapesOfHorror ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aThousandShapesOfHorror :: Difficulty -> AThousandShapesOfHorror
aThousandShapesOfHorror difficulty =
  scenario
    AThousandShapesOfHorror
    "06168"
    "A Thousand Shapes of Horror"
    difficulty
    [ "mysteriousStairs1 upstairsDoorway1   attic           upstairsDoorway2"
    , "mysteriousStairs2 .                  upstairsHallway ."
    , "mysteriousStairs3 downstairsDoorway1 frontPorch      downstairsDoorway2"
    , "mysteriousStairs4 .                  burialGround    unmarkedTomb"
    , "mysteriousStairs5 .                  .               ."
    ]

instance HasChaosTokenValue AThousandShapesOfHorror where
  getChaosTokenValue iid tokenFace (AThousandShapesOfHorror attrs) = case tokenFace of
    Skull -> do
      atGraveyard <- iid <=~> InvestigatorAt (LocationWithTrait Graveyard)
      pure $ toChaosTokenValue attrs Skull (if atGraveyard then 3 else 1) (if atGraveyard then 4 else 2)
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet (PositiveModifier $ byDifficulty attrs 2 1)
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 2 3
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
  , MinusFour
  , Skull
  , Skull
  , Cultist
  , ElderThing
  , ElderThing
  , AutoFail
  , ElderSign
  ]

instance RunMessage AThousandShapesOfHorror where
  runMessage msg s@(AThousandShapesOfHorror attrs) = runQueueT $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "dreamEaters.aThousandShapesOfHorror.intro1"
      atYourSide <- getHasRecord TheBlackCatIsAtYourSide
      story
        $ i18nWithTitle
        $ if atYourSide
          then "dreamEaters.aThousandShapesOfHorror.intro2"
          else "dreamEaters.aThousandShapesOfHorror.intro3"
      story $ i18nWithTitle "dreamEaters.aThousandShapesOfHorror.intro4"
      pure s
    StandaloneSetup -> do
      push $ SetChaosTokens standaloneChaosTokens
      pure s
    Setup -> runScenarioSetup AThousandShapesOfHorror attrs $ do
      gather Set.AThousandShapesOfHorror
      gather Set.CreaturesOfTheUnderworld
      gather Set.MergingRealities
      gather Set.ChillingCold
      gather Set.Ghouls
      gather Set.LockedDoors
      gather Set.Rats

      setAgendaDeck [Agendas.theHouseWithNoName, Agendas.theThingWithNoName, Agendas.theDeadWithNoName]
      setActDeck [Acts.searchingTheUnnamable, Acts.theEndlessStairs]

      startAt =<< place Locations.burialGround
      placeAll [Locations.frontPorchEntryway, Locations.upstairsHallway]

      placeGroup
        "downstairsDoorway"
        [Locations.downstairsDoorwayDen, Locations.downstairsDoorwayParlor]

      placeGroup
        "upstairsDoorway"
        [Locations.upstairsDoorwayBedroom, Locations.upstairsDoorwayLibrary]

      setAside
        [ Locations.attic_AThousandShapesOfHorror
        , Locations.unmarkedTomb
        , Locations.mysteriousStairs_183
        , Locations.mysteriousStairs_184
        , Locations.mysteriousStairs_185
        , Locations.mysteriousStairs_186
        , Locations.mysteriousStairs_187
        , Locations.mysteriousStairs_188
        , Treacheries.endlessDescent
        , Treacheries.endlessDescent
        , Treacheries.endlessDescent
        , Treacheries.endlessDescent
        , Assets.theSilverKey
        ]
    ResolveChaosToken _ Cultist iid -> do
      push $ DrawAnotherChaosToken iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist -> do
          mTheUnnamable <- selectOne $ enemyIs Enemies.theUnnamable
          for_ mTheUnnamable $ \theUnnamable -> do
            push $ InitiateEnemyAttack $ enemyAttack theUnnamable (ChaosTokenEffectSource Tablet) iid
        ElderThing -> do
          playerClueCount <- field InvestigatorClues iid
          player <- getPlayer iid
          let takeDamage = assignDamage iid (ChaosTokenEffectSource ElderThing) 1
          push
            $ if playerClueCount > 0
              then
                chooseOne
                  player
                  [ Label
                      "Place 1 of your clues on your location"
                      [InvestigatorPlaceCluesOnLocation iid (toSource attrs) 1]
                  , Label "Take 1 damage" [takeDamage]
                  ]
              else takeDamage
        _ -> pure ()
      pure s
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ n -> do
      case token.face of
        Tablet -> do
          enemies <-
            select (CanEvadeEnemy (ChaosTokenEffectSource Tablet))
              >>= filterM (fieldP EnemyFight (maybe False (<= n)))
          player <- getPlayer iid
          pushIfAny enemies
            $ chooseOne player [targetLabel enemy [Msg.EnemyEvaded iid enemy] | enemy <- enemies]
          pure ()
        _ -> pure ()
      pure s
    _ -> AThousandShapesOfHorror <$> lift (runMessage msg attrs)
