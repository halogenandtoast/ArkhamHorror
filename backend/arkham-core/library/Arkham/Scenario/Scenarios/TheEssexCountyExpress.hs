module Arkham.Scenario.Scenarios.TheEssexCountyExpress (
  TheEssexCountyExpress (..),
  theEssexCountyExpress,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Difficulty
import Arkham.Direction
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Helpers.Agenda
import Arkham.Helpers.Card
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message
import Arkham.Modifier
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheEssexCountyExpress.Story
import Arkham.Token
import Arkham.Trait qualified as Trait
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheEssexCountyExpress = TheEssexCountyExpress ScenarioAttrs
  deriving stock (Generic)
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theEssexCountyExpress :: Difficulty -> TheEssexCountyExpress
theEssexCountyExpress difficulty =
  scenario
    TheEssexCountyExpress
    "02159"
    "The Essex County Express"
    difficulty
    ["trainCar6 trainCar5 trainCar4 trainCar3 trainCar2 trainCar1 engineCar"]

instance HasChaosTokenValue TheEssexCountyExpress where
  getChaosTokenValue iid chaosTokenFace (TheEssexCountyExpress attrs) = case chaosTokenFace of
    Skull -> do
      step <- getCurrentAgendaStep
      pure $ toChaosTokenValue attrs Skull step (step + 1)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 1 0
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 3
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
  , Skull
  , Skull
  , Cultist
  , Tablet
  , ElderThing
  , AutoFail
  , ElderSign
  ]

readInvestigatorDefeat :: HasGame m => m [Message]
readInvestigatorDefeat = do
  defeatedInvestigatorIds <- selectList DefeatedInvestigator
  mNecronomiconOwner <- getOwner Assets.theNecronomiconOlausWormiusTranslation
  mDrHenryArmitageOwner <- getOwner Assets.drHenryArmitage
  mProfessorWarrenRiceOwner <- getOwner Assets.professorWarrenRice
  mDrFrancisMorganOwner <- getOwner Assets.drFrancisMorgan
  if null defeatedInvestigatorIds
    then pure []
    else do
      defeatedPlayers <- traverse getPlayer defeatedInvestigatorIds
      pure
        $ [story defeatedPlayers investigatorDefeat]
        <> [Record TheNecronomiconWasStolen | isJust mNecronomiconOwner]
        <> [RemoveCampaignCard Assets.theNecronomiconOlausWormiusTranslation]
        <> [Record DrHenryArmitageWasKidnapped | isJust mDrHenryArmitageOwner]
        <> [RemoveCampaignCard Assets.drHenryArmitage]
        <> [ Record ProfessorWarrenRiceWasKidnapped
           | isJust mProfessorWarrenRiceOwner
           ]
        <> [RemoveCampaignCard Assets.professorWarrenRice]
        <> [Record DrFrancisMorganWasKidnapped | isJust mDrFrancisMorganOwner]
        <> [RemoveCampaignCard Assets.drFrancisMorgan]
        <> [ AddCampaignCardToDeck iid Treacheries.acrossSpaceAndTime
           | iid <- defeatedInvestigatorIds
           ]

instance RunMessage TheEssexCountyExpress where
  runMessage msg s@(TheEssexCountyExpress attrs@ScenarioAttrs {..}) =
    case msg of
      SetChaosTokensForScenario -> do
        standalone <- getIsStandalone
        s <$ if standalone then push (SetChaosTokens standaloneChaosTokens) else pure ()
      Setup -> do
        players <- allPlayers

        engineCar <-
          sample
            ( Locations.engineCar_175
                :| [Locations.engineCar_176, Locations.engineCar_177]
            )

        trainCars <-
          take 6
            <$> shuffleM
              [ Locations.passengerCar_167
              , Locations.passengerCar_168
              , Locations.passengerCar_169
              , Locations.passengerCar_170
              , Locations.passengerCar_171
              , Locations.sleepingCar
              , Locations.diningCar
              , Locations.parlorCar
              ]

        encounterDeck <-
          buildEncounterDeck
            [ EncounterSet.TheEssexCountyExpress
            , EncounterSet.TheBeyond
            , EncounterSet.StrikingFear
            , EncounterSet.AncientEvils
            , EncounterSet.DarkCult
            ]

        (engineCarId, placeEngineCar) <- placeLocationCard engineCar
        placeTrainCars <- for (zip [6, 5 ..] trainCars) $ \(idx, car) -> do
          (locationId, placement) <- placeLocationCard car
          pure
            ( locationId
            ,
              [ placement
              , SetLocationLabel locationId ("trainCar" <> tshow @Int idx)
              ]
            )

        let
          start = fst . fromJustNote "No train cars?" $ headMay placeTrainCars
          end =
            fst
              . fromJustNote "No train cars?"
              $ headMay
                (reverse placeTrainCars)
          allCars = map fst placeTrainCars <> [engineCarId]
          token = case scenarioDifficulty of
            Easy -> MinusTwo
            Standard -> MinusThree
            Hard -> MinusFour
            Expert -> MinusFive

        pushAll
          $ [ story players intro
            , AddChaosToken token
            , SetEncounterDeck encounterDeck
            , SetAgendaDeck
            , SetActDeck
            ]
          <> concatMap snd placeTrainCars
          <> [ PlacedLocationDirection l1 LeftOf l2
             | (l1, l2) <- zip allCars (drop 1 allCars)
             ]
          <> [ placeEngineCar
             , PlacedLocationDirection engineCarId RightOf end
             , CreateWindowModifierEffect
                EffectSetupWindow
                (EffectModifiers [Modifier ScenarioSource Blank False])
                ScenarioSource
                (LocationTarget start)
             , RevealLocation Nothing start
             , MoveAllTo (toSource attrs) start
             ]

        setAsideCards <-
          genCards
            [ Treacheries.acrossSpaceAndTime
            , Treacheries.acrossSpaceAndTime
            , Treacheries.acrossSpaceAndTime
            , Treacheries.acrossSpaceAndTime
            ]
        agendas <-
          genCards
            [ Agendas.aTearInReality
            , Agendas.theMawWidens
            , Agendas.rollingBackwards
            , Agendas.drawnIn
            , Agendas.outOfTime
            ]
        acts <- genCards [Acts.run, Acts.getTheEngineRunning]

        TheEssexCountyExpress
          <$> runMessage
            msg
            ( attrs
                & (setAsideCardsL .~ setAsideCards)
                & (actStackL . at 1 ?~ acts)
                & (agendaStackL . at 1 ?~ agendas)
            )
      ResolveChaosToken _ Tablet iid | isEasyStandard attrs -> do
        closestCultists <-
          selectList
            $ NearestEnemyTo iid
            $ EnemyWithTrait
              Trait.Cultist
        player <- getPlayer iid
        s <$ case closestCultists of
          [] -> pure ()
          [x] -> push $ PlaceTokens (toSource attrs) (EnemyTarget x) Doom 1
          xs ->
            push
              $ chooseOne
                player
                [targetLabel x [PlaceTokens (toSource attrs) (EnemyTarget x) Doom 1] | x <- xs]
      ResolveChaosToken _ Tablet _ | isHardExpert attrs -> do
        cultists <- selectList $ EnemyWithTrait Trait.Cultist
        pushAll [PlaceTokens (toSource attrs) (EnemyTarget eid) Doom 1 | eid <- cultists]
        pure s
      FailedSkillTest iid _ _ (ChaosTokenTarget token) _ n -> do
        case chaosTokenFace token of
          Cultist -> pushAll [SetActions iid (toSource attrs) 0, ChooseEndTurn iid]
          ElderThing | isEasyStandard attrs -> do
            push $ toMessage $ chooseAndDiscardCard iid (ChaosTokenEffectSource ElderThing)
          ElderThing | isHardExpert attrs -> do
            pushAll $ replicate n $ toMessage $ chooseAndDiscardCard iid $ ChaosTokenEffectSource ElderThing
          _ -> pure ()
        pure s
      ScenarioResolution NoResolution ->
        s <$ pushAll [ScenarioResolution $ Resolution 2]
      ScenarioResolution (Resolution 1) -> do
        msgs <- readInvestigatorDefeat
        players <- allPlayers
        defeatedInvestigatorIds <- selectList DefeatedInvestigator
        xp <- getXp
        pushAll
          $ msgs
          <> [story players resolution1]
          <> [ GainXP
              iid
              (toSource attrs)
              (n + (if iid `elem` defeatedInvestigatorIds then 1 else 0))
             | (iid, n) <- xp
             ]
          <> [EndOfGame Nothing]
        pure s
      ScenarioResolution (Resolution 2) -> do
        msgs <- readInvestigatorDefeat
        players <- allPlayers
        defeatedInvestigatorIds <- selectList DefeatedInvestigator
        xp <- getXp
        pushAll
          $ msgs
          <> [ story players resolution2
             , Record TheInvestigatorsWereDelayedOnTheirWayToDunwich
             ]
          <> [ GainXP
              iid
              (toSource attrs)
              (n + (if iid `elem` defeatedInvestigatorIds then 1 else 0))
             | (iid, n) <- xp
             ]
          <> [EndOfGame Nothing]
        pure s
      _ -> TheEssexCountyExpress <$> runMessage msg attrs
