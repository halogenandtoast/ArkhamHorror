module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Scenarios.ReturnToTheVanishingOfElinaHarper (
  returnToTheVanishingOfElinaHarper,
) where

import Arkham.Act.CardDefs.TheInnsmouthConspiracy.TheVanishingOfElinaHarper qualified as Acts
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.FogOverInnsmouth qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Acts qualified as HBActs
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Assets qualified as HBAssets
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Enemies qualified as HBEnemies
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers (officialSetup, scenarioI18n)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Sets qualified as Sets
import Arkham.I18n
import Arkham.Matcher
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheInnsmouthConspiracy.TheVanishingOfElinaHarper
import Arkham.Scenarios.TheInnsmouthConspiracy.TheVanishingOfElinaHarper.Helpers hiding (
  scenarioI18n,
 )
import Arkham.Trait (Trait (Suspect))

newtype ReturnToTheVanishingOfElinaHarper
  = ReturnToTheVanishingOfElinaHarper TheVanishingOfElinaHarper
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue, HasModifiersFor)

returnToTheVanishingOfElinaHarper :: Difficulty -> ReturnToTheVanishingOfElinaHarper
returnToTheVanishingOfElinaHarper difficulty =
  scenarioWith
    (ReturnToTheVanishingOfElinaHarper . TheVanishingOfElinaHarper)
    ":return-to-the-innsmouth-conspiracy:022"
    "Return to The Vanishing of Elina Harper"
    difficulty
    scenarioLayout
    (referenceL .~ "07056")

instance RunMessage ReturnToTheVanishingOfElinaHarper where
  runMessage msg (ReturnToTheVanishingOfElinaHarper inner@(TheVanishingOfElinaHarper attrs)) =
    runQueueT $ scenarioI18n "returnToTheVanishingOfElinaHarper" $ case msg of
      Setup -> runScenarioSetup (ReturnToTheVanishingOfElinaHarper . TheVanishingOfElinaHarper) attrs do
        setIsReturnTo
        replaceSet Set.FogOverInnsmouth Sets.InnsmouthHaze
        replaceSet Set.LockedDoors Sets.BarricadedDoors
        substitute Acts.theSearchForAgentHarper HBActs.theSearchForAgentHarperV2
        -- Innsmouth Haze replaces Fog over Innsmouth, so its matching enemy stands in for
        -- the Winged One wherever the official scenario names one.
        substitute Enemies.wingedOne HBEnemies.immaterialOne

        setup $ ul do
          li.nested "gatherSets" do
            li.returnTo "replacedSets"
            li.returnTo "replacedCards"
          officialSetup "theVanishingOfElinaHarper" do
            li.nested "placeLocations" do
              li "startAt"
            li.nested "leadsDeck" do
              li "findingAgentHarper"
              li "splitPiles"
              li "chooseRandomly"
              li "shuffleRemaining"
          li.returnTo "hybridLeads"
          officialSetup "theVanishingOfElinaHarper" $ li "setAsideAgendaAndAct"
          li.returnTo "setAsideCards"
          unscoped $ li "shuffleRemainder"

        {- "Shuffle the \"Little\" Gemma, Ron Stalwick and Roderick story assets into the
        Leads deck." -}
        alsoInDeck LeadsDeck [HBAssets.littleGemma, HBAssets.ronStalwick, HBAssets.roderick]

        gather Sets.ReturnToTheVanishingOfElinaHarper
        setupTheVanishingOfElinaHarper attrs
      {- "While resolving one of Resolution 1 to 7, resolve the following as well: Your
      dealings with the townspeople have given you insight into what's going on here:
      Each investigator gains 1 extra experience for each Suspect enemy in the victory
      display." Awarded before delegating so it lands with the scenario's own XP. -}
      ScenarioResolution r | r `elem` map Resolution [1 .. 7] -> do
        -- `suspects` is already the Helpers' list of possible suspects, hence the name.
        suspectCount <- selectCount $ VictoryDisplayCardMatch $ basic $ #enemy <> CardWithTrait Suspect
        when (suspectCount > 0) do
          {- Runs after the resolution it hangs off, which is where the box's campaign guide
          prints it: the instruction, then the reward on the squared bullet. -}
          resolutionFlavor $ ul $ li.nested "extraResolution" $ li "townspeopleInsight"
          eachInvestigator \iid ->
            gainXp iid ScenarioSource (ikey "returnToTheInnsmouthConspiracy.xp.townspeopleInsight") suspectCount
        ReturnToTheVanishingOfElinaHarper <$> liftRunMessage msg inner
      _ -> ReturnToTheVanishingOfElinaHarper <$> liftRunMessage msg inner
