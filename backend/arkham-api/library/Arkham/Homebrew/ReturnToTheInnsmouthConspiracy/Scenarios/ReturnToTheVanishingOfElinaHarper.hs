module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Scenarios.ReturnToTheVanishingOfElinaHarper (
  returnToTheVanishingOfElinaHarper,
) where

import Arkham.Act.CardDefs.TheInnsmouthConspiracy.TheVanishingOfElinaHarper qualified as Acts
import Arkham.Agenda.CardDefs.TheInnsmouthConspiracy.TheVanishingOfElinaHarper qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.CardDefs.NightOfTheZealot.Nightgaunts qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Acts qualified as HBActs
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Assets qualified as HBAssets
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Enemies qualified as HBEnemies
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers (scenarioI18n)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Sets qualified as Sets
import Arkham.I18n
import Arkham.Id
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.TheVanishingOfElinaHarper qualified as Locations
import Arkham.Matcher
import Arkham.Message.Story (StoryMessage (..))
import Arkham.Placement
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheInnsmouthConspiracy.TheVanishingOfElinaHarper
import Arkham.Scenarios.TheInnsmouthConspiracy.TheVanishingOfElinaHarper.Helpers hiding (
  scenarioI18n,
 )
import Arkham.Story.CardDefs.TheInnsmouthConspiracy.TheVanishingOfElinaHarper qualified as Stories
import Arkham.Trait (Trait (Suspect))
import Arkham.Treachery.CardDefs.NightOfTheZealot.TheMidnightMasks qualified as Treacheries

newtype ReturnToTheVanishingOfElinaHarper
  = ReturnToTheVanishingOfElinaHarper TheVanishingOfElinaHarper
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToTheVanishingOfElinaHarper :: Difficulty -> ReturnToTheVanishingOfElinaHarper
returnToTheVanishingOfElinaHarper difficulty =
  scenarioWith
    (ReturnToTheVanishingOfElinaHarper . TheVanishingOfElinaHarper)
    ":return-to-the-innsmouth-conspiracy:022"
    "Return to The Vanishing of Elina Harper"
    difficulty
    []
    (referenceL .~ "07056")

instance RunMessage ReturnToTheVanishingOfElinaHarper where
  runMessage msg (ReturnToTheVanishingOfElinaHarper inner@(TheVanishingOfElinaHarper attrs)) =
    runQueueT $ scenarioI18n "returnToTheVanishingOfElinaHarper" $ case msg of
      Setup -> runScenarioSetup (ReturnToTheVanishingOfElinaHarper . TheVanishingOfElinaHarper) attrs do
        setIsReturnTo
        replaceSet Set.FogOverInnsmouth Sets.InnsmouthHaze
        replaceSet Set.LockedDoors Sets.BarricadedDoors
        substitute Acts.theSearchForAgentHarper HBActs.theSearchForAgentHarperV2

        setup $ ul do
          li "gatherSets"
          li "replacedSets"
          li "hybridLeads"
          unscoped $ li "shuffleRemainder"

        gather Sets.ReturnToTheVanishingOfElinaHarper
        gather Set.TheVanishingOfElinaHarper
        gather Set.AgentsOfDagon
        gather Set.FogOverInnsmouth
        gather Set.TheLocals
        gather Set.ChillingCold
        gather Set.LockedDoors
        gather Set.Nightgaunts
        gatherJust Set.TheMidnightMasks [Treacheries.falseLead, Treacheries.huntingShadow]

        setAgendaDeck [Agendas.decrepitDecay, Agendas.growingSuspicion]
        setActDeck [Acts.theSearchForAgentHarper]

        startAt =<< place Locations.innsmouthSquare

        placeAll
          [ Locations.marshRefinery
          , Locations.innsmouthHarbour
          , Locations.fishStreetBridge
          , Locations.firstNationalGrocery
          , Locations.gilmanHouse
          , Locations.theLittleBookshop
          ]

        (hideout, remainingHideouts) <- sampleWithRest hideouts
        (kidnapper, remainingSuspects) <- sampleWithRest suspects

        excludeFromEncounterDeck [hideout, kidnapper]
        -- "Shuffle the \"Little\" Gemma, Ron Stalwick and Roderick story assets into the
        -- Leads deck." They are gathered with the set, so they have to leave the encounter
        -- deck before joining the Leads deck.
        let hybrids = [HBAssets.littleGemma, HBAssets.ronStalwick, HBAssets.roderick]
        excludeFromEncounterDeck hybrids
        addExtraDeck LeadsDeck =<< shuffle (remainingHideouts <> remainingSuspects <> hybrids)

        setAside
          [ Agendas.franticPursuit
          , Acts.theRescue
          , Assets.thomasDawsonSoldierInANewWar
          , Assets.elinaHarperKnowsTooMuch
          , Enemies.huntingNightgaunt
          , Enemies.huntingNightgaunt
          , -- Innsmouth Haze replaces Fog over Innsmouth, so its matching card stands in
            -- for the Winged One the official setup sets aside (FAQ v2.0).
            HBEnemies.immaterialOne
          ]

        findingAgentHarper <- genCard Stories.findingAgentHarper
        push $ PlaceStory findingAgentHarper Global
        let target = StoryTarget $ StoryId $ coerce $ toCardCode findingAgentHarper
        kidnapperCard <- genCard kidnapper
        hideoutCard <- genCard hideout
        placeUnderneath target [kidnapperCard, hideoutCard]
        setMeta $ Meta {kidnapper = kidnapperCard, hideout = hideoutCard}
      {- "While resolving one of Resolution 1 to 7, resolve the following as well: Your
      dealings with the townspeople have given you insight into what's going on here:
      Each investigator gains 1 extra experience for each Suspect enemy in the victory
      display." Awarded before delegating so it lands with the scenario's own XP. -}
      ScenarioResolution r | r `elem` map Resolution [1 .. 7] -> do
        -- `suspects` is already the Helpers' list of possible suspects, hence the name.
        suspectCount <-
          selectCount $ VictoryDisplayCardMatch $ basic $ #enemy <> CardWithTrait Suspect
        when (suspectCount > 0) do
          story $ i18nWithTitle "townspeopleInsight"
          eachInvestigator \iid ->
            gainXp iid ScenarioSource (ikey "returnToTheInnsmouthConspiracy.xp.townspeopleInsight") suspectCount
        ReturnToTheVanishingOfElinaHarper <$> liftRunMessage msg inner
      _ -> ReturnToTheVanishingOfElinaHarper <$> liftRunMessage msg inner
