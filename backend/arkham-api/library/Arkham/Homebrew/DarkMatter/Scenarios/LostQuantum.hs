module Arkham.Homebrew.DarkMatter.Scenarios.LostQuantum (lostQuantum) where

import Arkham.Card (toCardCode)
import Arkham.Deck (DeckSignifier (EncounterDeck))
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (allInvestigators)
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Helpers
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.Matcher hiding (EncounterDeck, InvestigatorDefeated)
import Arkham.Message.Lifted.Log
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted

newtype LostQuantum = LostQuantum ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostQuantum :: Difficulty -> LostQuantum
lostQuantum difficulty =
  scenario
    LostQuantum
    ":dark-matter:089"
    "Lost Quantum"
    difficulty
    [ "equals . . ."
    , ". moon hourglass triangle"
    , "diamond trefoil circle ."
    , ". square . ."
    ]

instance HasChaosTokenValue LostQuantum where
  getChaosTokenValue iid tokenFace (LostQuantum attrs) = case tokenFace of
    Skull -> do
      n <- getFacedownCardCount iid
      pure $ ChaosTokenValue Skull (NegativeModifier n)
    Cultist -> do
      n <- selectCount Anywhere
      pure $ ChaosTokenValue Cultist (NegativeModifier $ byDifficulty attrs (n `div` 2) n)
    Tablet -> pure $ toChaosTokenValue attrs Tablet 1 2
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage LostQuantum where
  runMessage msg s@(LostQuantum attrs) = runQueueT $ scenarioI18n "lostQuantum" $ case msg of
    PreScenarioSetup -> do
      flavor $ scope "intro" $ h "title" >> p "body"
      pure s
    Setup -> runScenarioSetup LostQuantum attrs do
      setup $ ul do
        li "gatherSets"
        li "setAsideErwinSimmons"
        li "setAsideFeasterFromAfar"
        li "createScanningDeck"
        li.nested "placeLandingCraft" do
          li "startAt"
        li "randomizeAgenda"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      gather Set.LostQuantum
      gather Set.DeepSpace
      setAside [Assets.erwinSimmonsFading, Assets.erwinSimmonsQuantumPhysicist]
      setAside [Enemies.theFeasterFromAfar]
      setAside [Acts.destabilization]
      -- Place Landing Craft before constructing the scanning deck so its card
      -- is consumed from the gathered pool rather than copied into that deck.
      startAt =<< place Locations.landingCraft
      addScanningDeck
      agendas <-
        shuffle
          [ Agendas.theQuantumMaelstrom_091
          , Agendas.theQuantumMaelstrom_092
          , Agendas.theQuantumMaelstrom_093
          ]
      setAgendaDeck agendas
      setActDeck [Acts.elbrusStation, Acts.quantumZeno]
    DrewCards iid drew | drew.deck == EncounterDeck && null drew.cards -> do
      drewFacedown <- drawRandomFacedownCard iid
      unless drewFacedown $ push $ InvestigatorDefeated (toSource attrs) iid
      pure s
    ResolveChaosToken _ Tablet iid -> do
      drawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ ElderThing iid -> do
      void $ drawRandomFacedownCard iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      when (token.face == Tablet) $ placeFacedownInThreatArea iid 1
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolution "noResolution"
          let erwinResigned = toCardCode Assets.erwinSimmonsFading `elem` attrs.resignedCardCodes
          push $ ScenarioResolution $ Resolution $ if erwinResigned then 3 else 1
        Resolution 1 -> do
          record TheElbrusStationHasBeenLostInTheQuantumRealm
          record YouHaveWitnessedThePrimordialChaos
          addImpendingDoom 4
          earnXp attrs "resolution1"
        Resolution 2 -> do
          record TheElbrusStationHasBeenFullyStabilized
          addChaosToken ElderThing
          addImpendingDoom 1
          earnXp attrs "resolution2"
        Resolution 3 -> do
          record TheElbrusStationHasBeenLostInTheQuantumRealm
          iids <- allInvestigators
          addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.erwinSimmonsQuantumPhysicist
          addImpendingDoom 2
          earnXp attrs "resolution3"
        _ -> error "invalid resolution"
      when (r /= NoResolution) endOfScenario
      pure s
    _ -> LostQuantum <$> liftRunMessage msg attrs
