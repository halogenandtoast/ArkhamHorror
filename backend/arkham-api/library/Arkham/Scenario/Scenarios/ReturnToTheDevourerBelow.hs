module Arkham.Scenario.Scenarios.ReturnToTheDevourerBelow where

import Arkham.Campaigns.NightOfTheZealot.Key
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation
import Arkham.Helpers.Log
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher (CardMatcher (..))
import Arkham.Message.Lifted hiding (setActDeck, setAgendaDeck)
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Import.Lifted hiding (placeLocationCard, story)
import Arkham.Scenario.Scenarios.TheDevourerBelow
import Arkham.Scenarios.TheDevourerBelow.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype ReturnToTheDevourerBelow = ReturnToTheDevourerBelow TheDevourerBelow
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq, HasChaosTokenValue)

returnToTheDevourerBelow :: Difficulty -> ReturnToTheDevourerBelow
returnToTheDevourerBelow difficulty =
  scenarioWith
    (ReturnToTheDevourerBelow . TheDevourerBelow)
    "50032"
    "The Devourer Below"
    difficulty
    [ "woods1     .     woods2"
    , "woods1 mainPath woods2"
    , "woods3 mainPath woods4"
    , "woods3 ritualSite woods4"
    , "   .   ritualSite   .  "
    ]
    (referenceL .~ "01142")

instance RunMessage ReturnToTheDevourerBelow where
  runMessage msg s@(ReturnToTheDevourerBelow theDevourerBelow'@(TheDevourerBelow attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup -> runScenarioSetup (ReturnToTheDevourerBelow . TheDevourerBelow) attrs do
      gather EncounterSet.ReturnToTheDevourerBelow
      gather EncounterSet.TheDevourerBelow
      gather EncounterSet.AncientEvils
      gather EncounterSet.StrikingFear
      gather EncounterSet.GhoulsOfUmordhoth
      gather EncounterSet.TheDevourersCult
      gatherOneOf
        $ EncounterSet.AgentsOfYogSothoth
        :| [EncounterSet.AgentsOfShubNiggurath, EncounterSet.AgentsOfCthulhu, EncounterSet.AgentsOfHastur]

      setAside [Locations.ritualSite, Enemies.umordhoth]
      whenHasRecord GhoulPriestIsStillAlive $ addToEncounterDeck (Only Enemies.ghoulPriest)

      setActDeck actDeck
      setAgendaDeck agendaDeck
      addChaosToken ElderThing

      startAt =<< place Locations.mainPath
      placeGroupChooseN 4 "woods"
        $ Locations.arkhamWoodsUnhallowedGround
        :| [ Locations.arkhamWoodsTwistingPaths
           , Locations.arkhamWoodsOldHouse
           , Locations.arkhamWoodsCliffside
           , Locations.arkhamWoodsTangledThicket
           , Locations.arkhamWoodsQuietGlade
           , Locations.arkhamWoodsGreatWillow
           , Locations.arkhamWoodsLakeside
           , Locations.arkhamWoodsCorpseRiddenClearing
           , Locations.arkhamWoodsWoodenBridge
           ]

      cultistsWhoGotAway <- getRecordedCardCodes CultistsWhoGotAway
      placeDoomOnAgenda $ (length cultistsWhoGotAway + 1) `div` 2

      whenHasRecord ItIsPastMidnight
        $ pushAll
          [ AllRandomDiscard (toSource attrs) AnyCard
          , AllRandomDiscard (toSource attrs) AnyCard
          ]
    CreateEnemy creation@(enemyCreationMethod -> SpawnAtLocation lid) | toCardCode (enemyCreationCard creation) == "01157" -> do
      name <- field LocationName lid
      when (name == "Ritual Site") $ do
        vaultOfEarthlyDemise <- genCard Treacheries.vaultOfEarthlyDemise
        tid <- getRandom
        push $ AttachStoryTreacheryTo tid vaultOfEarthlyDemise (toTarget $ enemyCreationEnemyId creation)
      pure s
    _ -> ReturnToTheDevourerBelow <$> lift (runMessage msg theDevourerBelow')
