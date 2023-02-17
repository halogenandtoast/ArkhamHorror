module Arkham.Scenario.Scenarios.TheEssexCountyExpress
  ( TheEssexCountyExpress(..)
  , theEssexCountyExpress
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Direction
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.EncounterSet qualified as EncounterSet
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Agenda
import Arkham.Helpers.Card
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding ( RevealLocation )
import Arkham.Message
import Arkham.Modifier
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheEssexCountyExpress.Story
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Trait qualified as Trait
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheEssexCountyExpress = TheEssexCountyExpress ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theEssexCountyExpress :: Difficulty -> TheEssexCountyExpress
theEssexCountyExpress difficulty = scenario
  TheEssexCountyExpress
  "02159"
  "The Essex County Express"
  difficulty
  ["trainCar6 trainCar5 trainCar4 trainCar3 trainCar2 trainCar1 engineCar"]

instance HasTokenValue TheEssexCountyExpress where
  getTokenValue iid tokenFace (TheEssexCountyExpress attrs) = case tokenFace of
    Skull -> do
      step <- getCurrentAgendaStep
      pure $ toTokenValue attrs Skull step (step + 1)
    Cultist -> pure $ toTokenValue attrs Cultist 1 0
    Tablet -> pure $ toTokenValue attrs Tablet 2 4
    ElderThing -> pure $ toTokenValue attrs ElderThing 3 3
    otherFace -> getTokenValue iid otherFace attrs

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
    else
      pure
      $ [story defeatedInvestigatorIds investigatorDefeat]
      <> [ Record TheNecronomiconWasStolen | isJust mNecronomiconOwner ]
      <> [RemoveCampaignCard Assets.theNecronomiconOlausWormiusTranslation]
      <> [ Record DrHenryArmitageWasKidnapped | isJust mDrHenryArmitageOwner ]
      <> [RemoveCampaignCard Assets.drHenryArmitage]
      <> [ Record ProfessorWarrenRiceWasKidnapped
         | isJust mProfessorWarrenRiceOwner
         ]
      <> [RemoveCampaignCard Assets.professorWarrenRice]
      <> [ Record DrFrancisMorganWasKidnapped | isJust mDrFrancisMorganOwner ]
      <> [RemoveCampaignCard Assets.drFrancisMorgan]
      <> [ AddCampaignCardToDeck iid Treacheries.acrossSpaceAndTime
         | iid <- defeatedInvestigatorIds
         ]

instance RunMessage TheEssexCountyExpress where
  runMessage msg s@(TheEssexCountyExpress attrs@ScenarioAttrs {..}) =
    case msg of
      SetTokensForScenario -> do
        standalone <- getIsStandalone
        s <$ if standalone then push (SetTokens standaloneTokens) else pure ()
      Setup -> do
        investigatorIds <- allInvestigatorIds

        engineCar <- sample
          (Locations.engineCar_175
          :| [Locations.engineCar_176, Locations.engineCar_177]
          )

        trainCars <- take 6 <$> shuffleM
          [ Locations.passengerCar_167
          , Locations.passengerCar_168
          , Locations.passengerCar_169
          , Locations.passengerCar_170
          , Locations.passengerCar_171
          , Locations.sleepingCar
          , Locations.diningCar
          , Locations.parlorCar
          ]

        encounterDeck <- buildEncounterDeck
          [ EncounterSet.TheEssexCountyExpress
          , EncounterSet.TheBeyond
          , EncounterSet.StrikingFear
          , EncounterSet.AncientEvils
          , EncounterSet.DarkCult
          ]

        (engineCarId, placeEngineCar) <- placeLocationCard engineCar
        placeTrainCars <- for (zip [6, 5 ..] trainCars) $ \(idx, car) -> do
          (locationId, placement) <- placeLocationCard car
          pure (locationId, [placement, SetLocationLabel locationId ("trainCar" <> tshow @Int idx)])

        let
          start = fst . fromJustNote "No train cars?" $ headMay placeTrainCars
          end = fst . fromJustNote "No train cars?" $ headMay (reverse placeTrainCars)
          allCars = map fst placeTrainCars <> [engineCarId]
          token = case scenarioDifficulty of
            Easy -> MinusTwo
            Standard -> MinusThree
            Hard -> MinusFour
            Expert -> MinusFive


        pushAll
          $ [ story investigatorIds intro
            , AddToken token
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

        setAsideCards <- traverse
          genCard
          [ Treacheries.acrossSpaceAndTime
          , Treacheries.acrossSpaceAndTime
          , Treacheries.acrossSpaceAndTime
          , Treacheries.acrossSpaceAndTime
          ]

        TheEssexCountyExpress <$> runMessage
          msg
          (attrs
          & (setAsideCardsL .~ setAsideCards)
          & (actStackL . at 1 ?~ [Acts.run, Acts.getTheEngineRunning])
          & (agendaStackL
            . at 1
            ?~ [ Agendas.aTearInReality
               , Agendas.theMawWidens
               , Agendas.rollingBackwards
               , Agendas.drawnIn
               , Agendas.outOfTime
               ]
            )
          )
      ResolveToken _ Tablet iid | isEasyStandard attrs -> do
        closestCultists <- selectList $ NearestEnemyTo iid $ EnemyWithTrait
          Trait.Cultist
        s <$ case closestCultists of
          [] -> pure ()
          [x] -> push $ PlaceDoom (EnemyTarget x) 1
          xs -> push $ chooseOne
            iid
            [ targetLabel x [PlaceDoom (EnemyTarget x) 1] | x <- xs ]
      ResolveToken _ Tablet _ | isHardExpert attrs -> do
        cultists <- selectList $ EnemyWithTrait Trait.Cultist
        s <$ pushAll [ PlaceDoom (EnemyTarget eid) 1 | eid <- cultists ]
      FailedSkillTest iid _ _ (TokenTarget token) _ n ->
        s <$ case tokenFace token of
          Cultist ->
            pushAll [SetActions iid (toSource attrs) 0, ChooseEndTurn iid]
          ElderThing | isEasyStandard attrs -> push $ ChooseAndDiscardCard iid (TokenEffectSource ElderThing)
          ElderThing | isHardExpert attrs ->
            pushAll $ replicate n (ChooseAndDiscardCard iid (TokenEffectSource ElderThing))
          _ -> pure ()
      ScenarioResolution NoResolution ->
        s <$ pushAll [ScenarioResolution $ Resolution 2]
      ScenarioResolution (Resolution 1) -> do
        msgs <- readInvestigatorDefeat
        iids <- allInvestigatorIds
        defeatedInvestigatorIds <- selectList DefeatedInvestigator
        xp <- getXp
        pushAll
          $ msgs
          <> [story iids resolution1]
          <> [ GainXP
                 iid
                 (n + (if iid `elem` defeatedInvestigatorIds then 1 else 0))
             | (iid, n) <- xp
             ]
          <> [EndOfGame Nothing]
        pure s
      ScenarioResolution (Resolution 2) -> do
        msgs <- readInvestigatorDefeat
        iids <- allInvestigatorIds
        defeatedInvestigatorIds <- selectList DefeatedInvestigator
        xp <- getXp
        pushAll
          $ msgs
          <> [ story iids resolution2
             , Record TheInvestigatorsWereDelayedOnTheirWayToDunwich
             ]
          <> [ GainXP
                 iid
                 (n + (if iid `elem` defeatedInvestigatorIds then 1 else 0))
             | (iid, n) <- xp
             ]
          <> [EndOfGame Nothing]
        pure s
      _ -> TheEssexCountyExpress <$> runMessage msg attrs
