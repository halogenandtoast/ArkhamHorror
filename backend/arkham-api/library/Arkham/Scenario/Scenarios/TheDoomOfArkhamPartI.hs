module Arkham.Scenario.Scenarios.TheDoomOfArkhamPartI (theDoomOfArkhamPartI) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Campaign.Import.Lifted (setNextCampaignStep)
import Arkham.Campaigns.TheDrownedCity.CampaignSteps (pattern TheDoomOfArkhamPartII)
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.ChaosToken
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers.Xp
import Arkham.I18n
import Arkham.Matcher
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheDoomOfArkhamPartI.Helpers

newtype TheDoomOfArkhamPartI = TheDoomOfArkhamPartI ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theDoomOfArkhamPartI :: Difficulty -> TheDoomOfArkhamPartI
theDoomOfArkhamPartI difficulty = scenario TheDoomOfArkhamPartI "11682" "The Doom of Arkham Pt I" difficulty []

instance HasChaosTokenValue TheDoomOfArkhamPartI where
  getChaosTokenValue iid chaosTokenFace (TheDoomOfArkhamPartI attrs) = case chaosTokenFace of
    Skull -> do
      -- X = the number of locations with no scenario cards beneath them.
      n <- selectCount $ LocationWithCardsUnderneath NoCards
      pure $ ChaosTokenValue Skull (NegativeModifier $ byDifficulty attrs ((n + 1) `div` 2) n)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 3 5
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 5
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 2 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheDoomOfArkhamPartI where
  runMessage msg s@(TheDoomOfArkhamPartI attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      setChaosTokens (chaosBagContents attrs.difficulty)
      pure s
    Setup -> runScenarioSetup TheDoomOfArkhamPartI attrs do
      gather Set.TheDoomOfArkhamPartI
      gather Set.TheMidnightMasks

      setActDeck [Acts.thePhantomShop]
      setAgendaDeck [Agendas.theComingStorm]

      -- TODO: place the Arkham locations from The Midnight Masks per the diagram and
      -- start the investigators at Rivertown; the phantom Tillinghast Esoterica (with
      -- the earned Artifact stack beneath it) is revealed adjacent via its own card.
      pure ()
    ScenarioResolution res -> scope "resolutions" do
      case res of
        Resolution 1 -> resolutionWithXp "resolution1" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        Resolution 2 -> resolutionWithXp "resolution2" $ allGainXpWithBonus' attrs $ toBonus "bonus" 1
        NoResolution -> resolutionWithXp "noResolution" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        _ -> error $ "Unknown resolution: " <> show res
      -- Both paths lead immediately into the final confrontation.
      setNextCampaignStep TheDoomOfArkhamPartII
      endOfScenario
      pure s
    _ -> TheDoomOfArkhamPartI <$> liftRunMessage msg attrs
