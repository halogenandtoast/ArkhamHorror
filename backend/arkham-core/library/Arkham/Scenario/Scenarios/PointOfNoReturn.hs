{-# LANGUAGE MultiWayIf #-}

module Arkham.Scenario.Scenarios.PointOfNoReturn (PointOfNoReturn (..), pointOfNoReturn) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Attack
import Arkham.CampaignLogKey
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Log
import Arkham.Helpers.Scenario
import Arkham.Helpers.SkillTest (getSkillTestAction)
import Arkham.Location.Cards (cragOfTheGhouls)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Cards.PlainOfTheGhouls (plainOfTheGhouls)
import Arkham.Matcher
import Arkham.Message.Lifted hiding (setActDeck, setAgendaDeck)
import Arkham.Prelude
import Arkham.Scenario.Runner hiding (chooseOne, story)
import Arkham.Scenario.Setup
import Arkham.ScenarioLogKey
import Arkham.Treachery.Cards qualified as Treacheries

newtype PointOfNoReturn = PointOfNoReturn ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pointOfNoReturn :: Difficulty -> PointOfNoReturn
pointOfNoReturn difficulty =
  scenario
    PointOfNoReturn
    "06247"
    "Point of No Return"
    difficulty
    [ "enchantedWoodsStoneTrapdoor towerOfKoth     seaOfBones  peaksOfThok seaOfPitch1"
    , "vaultsOfZin                 cityOfGugs      valeOfPnath seaOfPitch2 ."
    , "plainOfTheGhouls            cragOfTheGhouls .           seaOfPitch3 seaOfPitch4"
    ]

instance HasChaosTokenValue PointOfNoReturn where
  getChaosTokenValue iid tokenFace (PointOfNoReturn attrs) = case tokenFace of
    Skull -> do
      n <- scenarioCount Distortion
      pure $ toChaosTokenValue attrs Skull n (n + 1)
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet (PositiveModifier $ byDifficulty attrs 1 0)
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
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
  , Skull
  , Cultist
  , ElderThing
  , ElderThing
  , AutoFail
  , ElderSign
  ]

instance RunMessage PointOfNoReturn where
  runMessage msg s@(PointOfNoReturn attrs) = runQueueT $ case msg of
    PreScenarioSetup -> do
      randolphDidNotSurvive <- getHasRecord RandolphDidNotSurviveTheDescent
      if randolphDidNotSurvive
        then story $ i18nWithTitle "dreamEaters.pointOfNoReturn.intro1"
        else story $ i18nWithTitle "dreamEaters.pointOfNoReturn.intro2"
      pure s
    StandaloneSetup -> do
      record RandolphDidNotSurviveTheDescent
      setChaosTokens standaloneChaosTokens
      pure s
    Setup -> runScenarioSetup PointOfNoReturn attrs $ do
      gather Set.PointOfNoReturn
      gather Set.CreaturesOfTheUnderworld
      gather Set.WhispersOfHypnos
      gather Set.AncientEvils
      gather Set.Ghouls
      gather Set.StrikingFear

      gatherAndSetAside Set.DescentIntoThePitch
      gatherAndSetAside Set.TerrorOfTheVale
      gatherAndSetAside Set.AgentsOfAtlachNacha
      gatherAndSetAside Set.Nightgaunts

      startAt =<< place Locations.vaultsOfZin
      placeAll [Locations.cityOfGugs, Locations.towerOfKoth, Locations.plainOfTheGhouls]

      setAside
        [ Locations.cragOfTheGhouls
        , Locations.seaOfBones
        , Locations.peaksOfThok
        , Locations.valeOfPnath
        , Locations.seaOfPitch_262
        , Locations.seaOfPitch_263
        , Locations.seaOfPitch_264
        , Locations.seaOfPitch_265
        , Locations.enchantedWoodsStoneTrapdoor
        , Enemies.gugSentinel
        , Assets.richardUptonPickman
        , Treacheries.falseAwakeningPointOfNoReturn
        ]

      setAgendaDeck [Agendas.aSinisterRealm, Agendas.besetByMonsters]

      randolphDidNotSurvive <- getHasRecord RandolphDidNotSurviveTheDescent
      let act1 = if randolphDidNotSurvive then Acts.enteringTheUnderworldV1 else Acts.enteringTheUnderworldV2

      setActDeck [act1, Acts.theDescent, Acts.theBlackExpanse]

      steps <- getRecordCount StepsOfTheBridge

      if
        | steps == 0 -> pure ()
        | steps <= 4 -> push $ ScenarioCountIncrementBy Distortion 1
        | otherwise -> push $ ScenarioCountIncrementBy Distortion 2
    ResolveChaosToken _ Cultist iid -> do
      push $ DrawAnotherChaosToken iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ n -> do
      case token.face of
        Cultist -> push $ InvestigatorDrawEncounterCard iid
        ElderThing | n >= 2 -> do
          enemies <-
            select
              $ ReadyEnemy
              <> oneOf
                [EnemyAt (locationWithInvestigator iid), EnemyAt $ ConnectedFrom (locationWithInvestigator iid)]
          chooseOne
            iid
            [ targetLabel
              enemy
              [EnemyEngageInvestigator enemy iid, InitiateEnemyAttack $ enemyAttack enemy attrs iid]
            | enemy <- enemies
            ]
        _ -> pure ()
      pure s
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case chaosTokenFace token of
        Tablet -> void $ runMaybeT $ do
          Action.Investigate <- MaybeT getSkillTestAction
          drawCard <- MaybeT $ drawCardsIfCan iid TabletEffect 1
          lift $ push drawCard
        _ -> pure ()
      pure s
    _ -> PointOfNoReturn <$> lift (runMessage msg attrs)
