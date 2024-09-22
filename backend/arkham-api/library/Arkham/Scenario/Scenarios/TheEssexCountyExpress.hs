module Arkham.Scenario.Scenarios.TheEssexCountyExpress (
  TheEssexCountyExpress (..),
  theEssexCountyExpress,
) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.ChaosToken
import Arkham.Direction
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers.Agenda
import Arkham.Helpers.Card
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Modifier
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheEssexCountyExpress.Story
import Arkham.Token
import Arkham.Trait qualified as Trait
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Xp

newtype TheEssexCountyExpress = TheEssexCountyExpress ScenarioAttrs
  deriving stock Generic
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

readInvestigatorDefeat :: ReverseQueue m => m ()
readInvestigatorDefeat = do
  defeated <- select DefeatedInvestigator
  storyOnly defeated investigatorDefeat
  withOwner Assets.theNecronomiconOlausWormiusTranslation \owner ->
    when (owner `elem` defeated) do
      record TheNecronomiconWasStolen
      removeCampaignCard Assets.theNecronomiconOlausWormiusTranslation
  withOwner Assets.drHenryArmitage \owner ->
    when (owner `elem` defeated) do
      record DrHenryArmitageWasKidnapped
      removeCampaignCard Assets.drHenryArmitage
  withOwner Assets.professorWarrenRice \owner ->
    when (owner `elem` defeated) do
      record ProfessorWarrenRiceWasKidnapped
      removeCampaignCard Assets.professorWarrenRice
  withOwner Assets.drFrancisMorgan \owner ->
    when (owner `elem` defeated) do
      record DrFrancisMorganWasKidnapped
      removeCampaignCard Assets.drFrancisMorgan
  for_ defeated \iid -> addCampaignCardToDeck iid Treacheries.acrossSpaceAndTime

instance RunMessage TheEssexCountyExpress where
  runMessage msg s@(TheEssexCountyExpress attrs) = runQueueT $ case msg of
    PreScenarioSetup -> do
      story intro
      pure s
    StandaloneSetup -> do
      setChaosTokens standaloneChaosTokens
      pure s
    Setup -> runScenarioSetup TheEssexCountyExpress attrs do
      trainCars <-
        sampleN 6
          $ Locations.passengerCar_167
          :| [ Locations.passengerCar_168
             , Locations.passengerCar_169
             , Locations.passengerCar_170
             , Locations.passengerCar_171
             , Locations.sleepingCar
             , Locations.diningCar
             , Locations.parlorCar
             ]

      gather Set.TheEssexCountyExpress
      gather Set.TheBeyond
      gather Set.StrikingFear
      gather Set.AncientEvils
      gather Set.DarkCult

      engineCar <-
        place =<< sampleOneOf (Locations.engineCar_175, Locations.engineCar_176, Locations.engineCar_177)

      placedCars <- for (zip [6, 5 ..] trainCars) $ \(idx, trainCarCard) -> do
        car <- place trainCarCard
        push $ SetLocationLabel car ("trainCar" <> tshow @Int idx)
        pure car

      let
        start = fromJustNote "No train cars?" $ headMay placedCars
        end = fromJustNote "No train cars?" $ headMay (reverse placedCars)
        allCars = placedCars <> [engineCar]
        token = case attrs.difficulty of
          Easy -> MinusTwo
          Standard -> MinusThree
          Hard -> MinusFour
          Expert -> MinusFive

      addChaosToken token
      pushAll
        [ PlacedLocationDirection l1 LeftOf l2
        | (l1, l2) <- zip allCars (drop 1 allCars)
        ]

      push $ PlacedLocationDirection engineCar RightOf end
      setupModifier ScenarioSource (LocationTarget start) Blank
      startAt start

      setAside
        [ Treacheries.acrossSpaceAndTime
        , Treacheries.acrossSpaceAndTime
        , Treacheries.acrossSpaceAndTime
        , Treacheries.acrossSpaceAndTime
        ]
      setAgendaDeck
        [ Agendas.aTearInReality
        , Agendas.theMawWidens
        , Agendas.rollingBackwards
        , Agendas.drawnIn
        , Agendas.outOfTime
        ]
      setActDeck [Acts.run, Acts.getTheEngineRunning]
    ResolveChaosToken _ Tablet iid | isEasyStandard attrs -> do
      closestCultists <- select $ NearestEnemyTo iid $ EnemyWithTrait Trait.Cultist
      case closestCultists of
        [] -> pure ()
        [x] -> push $ PlaceTokens (toSource attrs) (EnemyTarget x) Doom 1
        xs ->
          chooseOne
            iid
            [targetLabel x [PlaceTokens (toSource attrs) (EnemyTarget x) Doom 1] | x <- xs]
      pure s
    ResolveChaosToken _ Tablet _ | isHardExpert attrs -> do
      cultists <- select $ EnemyWithTrait Trait.Cultist
      pushAll [PlaceTokens (toSource attrs) (EnemyTarget eid) Doom 1 | eid <- cultists]
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ n -> do
      case chaosTokenFace token of
        Cultist -> pushAll [SetActions iid (toSource attrs) 0, ChooseEndTurn iid]
        ElderThing | isEasyStandard attrs -> do
          chooseAndDiscardCard iid (ChaosTokenEffectSource ElderThing)
        ElderThing | isHardExpert attrs -> do
          replicateM_ n $ chooseAndDiscardCard iid $ ChaosTokenEffectSource ElderThing
        _ -> pure ()
      pure s
    ScenarioResolution NoResolution -> do
      push R2
      pure s
    ScenarioResolution (Resolution 1) -> do
      readInvestigatorDefeat
      story resolution1
      defeatedInvestigatorIds <- select DefeatedInvestigator
      allGainXpWith attrs \iid ->
        guard (iid `elem` defeatedInvestigatorIds)
          *> [ InvestigatorGainXp
                iid
                (XpDetail XpBonus "His or her experience beyond the gate grants them insight into the cosmos" 1)
             ]
      endOfScenario
      pure s
    ScenarioResolution (Resolution 2) -> do
      readInvestigatorDefeat
      story resolution2
      record TheInvestigatorsWereDelayedOnTheirWayToDunwich
      defeatedInvestigatorIds <- select DefeatedInvestigator
      allGainXpWith attrs \iid ->
        guard (iid `elem` defeatedInvestigatorIds)
          *> [ InvestigatorGainXp
                iid
                (XpDetail XpBonus "His or her experience beyond the gate grants them insight into the cosmos" 1)
             ]
      endOfScenario
      pure s
    _ -> TheEssexCountyExpress <$> liftRunMessage msg attrs
