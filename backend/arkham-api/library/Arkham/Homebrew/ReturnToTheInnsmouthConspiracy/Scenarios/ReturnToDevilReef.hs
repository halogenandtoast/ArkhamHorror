module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Scenarios.ReturnToDevilReef (returnToDevilReef) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Campaigns.TheInnsmouthConspiracy.Memory
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers.FlavorText
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Assets qualified as HBAssets
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Locations qualified as HBLocations
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as HBTreacheries
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers (
  officialSetup,
  scenarioI18n,
 )
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Sets qualified as Sets
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.FloodedCaverns qualified as Locations
import Arkham.Location.Grid
import Arkham.Location.Types (Field (LocationClues))
import Arkham.Matcher hiding (assetAt)
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheInnsmouthConspiracy.DevilReef

newtype ReturnToDevilReef = ReturnToDevilReef DevilReef
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue, HasModifiersFor)

returnToDevilReef :: Difficulty -> ReturnToDevilReef
returnToDevilReef difficulty =
  scenarioWith
    (ReturnToDevilReef . DevilReef)
    ":return-to-the-innsmouth-conspiracy:031"
    "Return to DevilReef"
    difficulty
    []
    (referenceL .~ "07163")

instance RunMessage ReturnToDevilReef where
  runMessage msg s'@(ReturnToDevilReef inner@(DevilReef attrs)) =
    runQueueT $ scenarioI18n "returnToDevilReef" $ case msg of
      Setup -> runScenarioSetup (ReturnToDevilReef . DevilReef) attrs do
        setIsReturnTo
        replaceSet Set.RisingTide Sets.RollingTide
        substitute Assets.fishingVessel HBAssets.fishingVesselV2
        aBattle <- hasMemory ABattleWithAHorrifyingDevil

        setup $ ul do
          li.nested "gatherSets" do
            li.returnTo "replacedSets"
            li.returnTo "replacedCards"
          officialSetup "devilReef" do
            li.nested "placeKeys" do
              li "faceupKeys"
              li "facedownKeys"
            li.nested "churningWaters" do
              li "fishingVessel"
              li "startInVessel"
            li "setAsideRelics"
          li.returnTo "devilReefLocations"
          officialSetup "devilReef" $ li.nested "unfathomableDepths" do
            li "removeThree"
            li "setAsideThree"
          li.returnTo "tidalTunnels"
          officialSetup "devilReef" do
            li.nested "tidalTunnelsDeck" do
              li "unrevealedSide"
              li "placeNearEncounterDeck"
            li.nested "checkCampaignLog" do
              li.validate aBattle "secretsOfTheSeaV1"
              li.validate (not aBattle) "secretsOfTheSeaV2"
            li "floodTokens"
          unscoped $ li "shuffleRemainder"

        {- "Shuffle Cave Mouth in with the rest of the Devil Reef locations. Do not remove
        any; use all six." The sixth seat mirrors the northern one. -}
        alsoPlaceInGrid "devilReefLocations" (Pos 0 (-3)) HBLocations.caveMouth

        {- "Replace one of each Tidal Pool, Underground River and Underwater Cavern from
        the original Flooded Caverns set with its counterpart from the Return to Flooded
        Caverns set." -}
        replaceOneOf Locations.tidalPool HBLocations.tidalPool
        replaceOneOf Locations.undergroundRiver HBLocations.undergroundRiver
        replaceOneOf Locations.underwaterCavern HBLocations.underwaterCavern
        gather Sets.ReturnToDevilReef
        gather Sets.ReturnToFloodedCaverns
        setupDevilReef attrs
      {- "Scenario Interlude: A Bargain", reached from Shrine to Hydra's ability. Pay the
      price and Innsmouth Influence joins your deck -- which is what makes you a Deep One
      investigator for the rest of the campaign. -}
      Msg.ScenarioSpecific "aBargain" (maybeResult -> Just iid) -> do
        scope "aBargain" do
          let interlude = do
                h "title"
                p "intro"
                p "body"
                {- The two options read as the squared bullets the buttons use, which is what
                nesting them under the "Choose one:" line gives. -}
                ul $ li.nested "chooseOne" do
                  li "payThePrice"
                  li "backOut"
          investigatorStoryWithChooseOneM' iid interlude do
            labeled "payThePrice" $ do_ msg
            labeled "backOut" nothing
        pure s'
      Do (Msg.ScenarioSpecific "aBargain" (maybeResult -> Just iid)) -> do
        influence <- addCampaignCardToDeckCapture iid DoNotShuffleIn HBTreacheries.innsmouthInfluence
        createTreacheryAt_ influence (InThreatArea iid)
        withMatch (locationIs HBLocations.shrineToHydra) \lid -> do
          clues <- field LocationClues lid
          when (clues > 0) $ discoverAt NotInvestigate iid ScenarioSource clues lid
        gainResources iid ScenarioSource 5
        pure s'
      _ -> ReturnToDevilReef <$> liftRunMessage msg inner
