module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Scenarios.ReturnToALightInTheFog (returnToALightInTheFog) where

import Arkham.Agenda.CardDefs.TheInnsmouthConspiracy.ALightInTheFog qualified as Agendas
import Arkham.Campaigns.TheInnsmouthConspiracy.Key
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Log
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Agendas qualified as HBAgendas
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Enemies qualified as HBEnemies
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Locations qualified as HBLocations
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers (
  deepOneInvestigatorsInCampaign,
  officialSetup,
  scenarioI18n,
 )
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Sets qualified as Sets
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.FloodedCaverns qualified as Locations
import Arkham.Matcher
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheInnsmouthConspiracy.ALightInTheFog

newtype ReturnToALightInTheFog = ReturnToALightInTheFog ALightInTheFog
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue, HasModifiersFor)

returnToALightInTheFog :: Difficulty -> ReturnToALightInTheFog
returnToALightInTheFog difficulty =
  scenarioWith
    (ReturnToALightInTheFog . ALightInTheFog)
    ":return-to-the-innsmouth-conspiracy:039"
    "Return to ALightInTheFog"
    difficulty
    []
    (referenceL .~ "07231")

instance RunMessage ReturnToALightInTheFog where
  runMessage msg (ReturnToALightInTheFog inner@(ALightInTheFog attrs)) =
    runQueueT $ scenarioI18n "returnToALightInTheFog" $ case msg of
      Setup -> runScenarioSetup (ReturnToALightInTheFog . ALightInTheFog) attrs do
        setIsReturnTo
        replaceSet Set.Syzygy Sets.Occultation
        replaceSet Set.RisingTide Sets.RollingTide
        substitute Agendas.terrorAtFalconPoint HBAgendas.terrorAtFalconPointV2
        idolBrought <- getHasRecord TheIdolWasBroughtToTheLighthouse
        mantleBrought <- getHasRecord TheMantleWasBroughtToTheLighthouse
        headdressBrought <- getHasRecord TheHeaddressWasBroughtToTheLighthouse
        afterSunrise <- getHasRecord TheInvestigatorsReachedFalconPointAfterSunrise
        tideGrownStronger <- getHasRecord TheTideHasGrownStronger

        setup $ ul do
          li.nested "gatherSets" do
            li.returnTo "replacedSets"
            li.returnTo "replacedCards"
          li.returnTo "tidalTunnels"
          officialSetup "aLightInTheFog" do
            li.nested "placeLocations" do
              li "startAt"
              li "removeUndergroundRivers"
              li "setAsideOtherLocations"
            li.nested "placeKeys" do
              li "faceupKeys"
              li "facedownKeys"
            li "captured"
            li "setAsideCards"
          li.returnTo "setAsideGrapplers"
          officialSetup "aLightInTheFog" do
            li.nested "checkCampaignLog" do
              li.validate idolBrought "wavewornIdol"
              li.validate mantleBrought "awakenedMantle"
              li.validate headdressBrought "headdressOfYhaNthlei"
            li.nested "checkCampaignLogDoom" do
              li.validate afterSunrise "afterSunrise"
              li.validate tideGrownStronger "tideHasGrownStronger"
            li "floodTokens"
          unscoped $ li "shuffleRemainder"

        {- "Replace one of each Tidal Pool, Underground River and Underwater Cavern from
        the original Flooded Caverns set with its counterpart from the Return to Flooded
        Caverns set." -}
        replaceOneOf Locations.tidalPool HBLocations.tidalPool
        replaceOneOf Locations.undergroundRiver HBLocations.undergroundRiver
        replaceOneOf Locations.underwaterCavern HBLocations.underwaterCavern
        gather Sets.ReturnToALightInTheFog
        gather Sets.ReturnToFloodedCaverns
        {- The Campaign Guide removes the Underground Rivers from the tunnels; the box has
        its own, so it goes too. -}
        removeCards =<< amongGathered (cardIs HBLocations.undergroundRiver)
        setupALightInTheFog attrs

        -- "Set aside all three copies of Deep One Grappler, out of play."
        setAside $ replicate 3 HBEnemies.deepOneGrappler
      {- "While resolving one of Resolution 1, 2 or 4, every Deep One investigator has to
      read this: ... You gain 1 mental trauma as your mind tries to cope with the changes
      your body is going through." -}
      ScenarioResolution r | r `elem` map Resolution [1, 2, 4] -> do
        {- Innsmouth Influence is a permanent, so who is a Deep One survives the scenario
        ending: asking the campaign's story cards finds them even once they have resigned
        and their cards are gone, which asking for the trait does not. -}
        deepOnes <- deepOneInvestigatorsInCampaign
        inner' <- liftRunMessage msg inner
        unless (null deepOnes) do
          resolutionOnly deepOnes $ scope "changing" do
            setTitle "title"
            p "instructions"
            p "body"
            ul $ li "trauma"
          for_ deepOnes (`sufferMentalTrauma` 1)
        pure $ ReturnToALightInTheFog inner'
      _ -> ReturnToALightInTheFog <$> liftRunMessage msg inner
