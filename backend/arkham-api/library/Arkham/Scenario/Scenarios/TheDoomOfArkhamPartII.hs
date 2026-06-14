module Arkham.Scenario.Scenarios.TheDoomOfArkhamPartII (theDoomOfArkhamPartII) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.ChaosToken
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers.Xp
import Arkham.I18n
import Arkham.Message.Lifted.Log (record)
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers

newtype TheDoomOfArkhamPartII = TheDoomOfArkhamPartII ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theDoomOfArkhamPartII :: Difficulty -> TheDoomOfArkhamPartII
theDoomOfArkhamPartII difficulty =
  scenario TheDoomOfArkhamPartII "11688a" "The Doom of Arkham Pt II" difficulty []

instance HasChaosTokenValue TheDoomOfArkhamPartII where
  getChaosTokenValue iid chaosTokenFace (TheDoomOfArkhamPartII attrs) = case chaosTokenFace of
    Skull -> do
      rage <- getCthulhuRage
      pure $ ChaosTokenValue Skull (NegativeModifier $ byDifficulty attrs rage (rage + 2))
    Cultist -> pure $ toChaosTokenValue attrs Cultist 3 5
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 5
    -- TODO: elder thing draws the top card of the Cthulhu deck on a fail.
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 1 3
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheDoomOfArkhamPartII where
  runMessage msg s@(TheDoomOfArkhamPartII attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      setChaosTokens (chaosBagContents attrs.difficulty)
      pure s
    Setup -> runScenarioSetup TheDoomOfArkhamPartII attrs do
      gather Set.TheDoomOfArkhamPartII
      gather Set.TheMidnightMasks

      setActDeck [Acts.fightBack]
      setAgendaDeck [Agendas.theDoomOfArkham]

      -- TODO: this is the campaign's climax. The "Cthulhu Board" (the four facets —
      -- Ancient Evil + Hoary Wings / Fierce Visage / Wicked Claw, each with an Enraged
      -- back), the "Cthulhu deck" of action cards, the Ruined-location swaps, the
      -- sigils, and the Rage ratchet all lack engine support and are scaffolded here:
      -- place the Arkham locations, set up the Cthulhu Board with the four facets, and
      -- build the Cthulhu deck per the page-46 setup. Cthulhu's Rage starts at 0.
      setScenarioMeta initDoomMeta
    ScenarioSpecific "increaseCthulhuRage" _ -> do
      rage <- getCthulhuRage
      setScenarioMeta (DoomMeta (rage + 1))
      pure s
    ScenarioSpecific "setCthulhuRage" (toResultDefault (0 :: Int) -> rage) -> do
      setScenarioMeta (DoomMeta rage)
      pure s
    ScenarioResolution res -> scope "resolutions" do
      case res of
        -- Cthulhu is banished/driven away for good.
        Resolution 1 -> do
          record CthulhuWasBanished
          resolutionWithXp "resolution1" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        Resolution 2 -> do
          record CthulhuWasBanished
          resolutionWithXp "resolution2" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        Resolution 3 -> do
          record CthulhuWasDrivenAway
          resolutionWithXp "resolution3" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        NoResolution -> do
          record CthulhuAnnihilatedTheExpedition
          resolutionWithXp "noResolution" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        _ -> error $ "Unknown resolution: " <> show res
      endOfScenario
      pure s
    _ -> TheDoomOfArkhamPartII <$> liftRunMessage msg attrs
