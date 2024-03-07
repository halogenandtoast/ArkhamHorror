module Arkham.Scenario.Scenarios.AThousandShapesOfHorror (
  AThousandShapesOfHorror (..),
  aThousandShapesOfHorror,
) where

import Arkham.Prelude hiding ((.=))

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers.Log
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Message.Lifted
import Arkham.Scenario.Runner hiding (placeLocationCard, pushAll, story)
import Arkham.Scenario.Setup
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
    [ "upstairsDoorway1   .               upstairsDoorway2"
    , ".                  upstairsHallway ."
    , "downstairsDoorway1 frontPorch      downstairsDoorway2"
    , ".                  burialGround    ."
    ]

instance HasChaosTokenValue AThousandShapesOfHorror where
  getChaosTokenValue iid tokenFace (AThousandShapesOfHorror attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
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

      burialGround <- place Locations.burialGround
      placeAll [Locations.frontPorchEntryway, Locations.upstairsHallway]

      placeGroup
        "downstairsDoorway"
        [Locations.downstairsDoorwayDen, Locations.downstairsDoorwayParlor]

      placeGroup
        "upstairsDoorway"
        [Locations.upstairsDoorwayBedroom, Locations.upstairsDoorwayLibrary]

      startAt burialGround

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
    _ -> AThousandShapesOfHorror <$> lift (runMessage msg attrs)
