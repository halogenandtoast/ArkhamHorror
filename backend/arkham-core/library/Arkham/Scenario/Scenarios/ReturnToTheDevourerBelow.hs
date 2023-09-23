module Arkham.Scenario.Scenarios.ReturnToTheDevourerBelow where

import Arkham.Prelude

import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher (CardMatcher (..))
import Arkham.Message
import Arkham.Projection
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenario.Scenarios.TheDevourerBelow
import Arkham.Scenarios.TheDevourerBelow.Story
import Arkham.Treachery.Cards qualified as Treacheries

newtype ReturnToTheDevourerBelow = ReturnToTheDevourerBelow TheDevourerBelow
  deriving stock (Generic)
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

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

instance HasChaosTokenValue ReturnToTheDevourerBelow where
  getChaosTokenValue iid chaosTokenFace (ReturnToTheDevourerBelow theDevourerBelow') =
    getChaosTokenValue iid chaosTokenFace theDevourerBelow'

instance RunMessage ReturnToTheDevourerBelow where
  runMessage msg s@(ReturnToTheDevourerBelow theDevourerBelow'@(TheDevourerBelow attrs)) =
    case msg of
      Setup -> do
        investigatorIds <- allInvestigatorIds
        pastMidnight <- getHasRecord ItIsPastMidnight
        ghoulPriestAlive <- getHasRecord GhoulPriestIsStillAlive
        cultistsWhoGotAway <- getRecordSet CultistsWhoGotAway
        ghoulPriestCard <- genEncounterCard Enemies.ghoulPriest

        let
          woodsLabels = ["woods1", "woods2", "woods3", "woods4"]
          ghoulPriestMessages =
            [AddToEncounterDeck ghoulPriestCard | ghoulPriestAlive]
          pastMidnightMessages =
            if pastMidnight
              then
                [ AllRandomDiscard (toSource attrs) AnyCard
                , AllRandomDiscard (toSource attrs) AnyCard
                ]
              else []
          cultistsWhoGotAwayMessages =
            replicate
              ((length cultistsWhoGotAway + 1) `div` 2)
              PlaceDoomOnAgenda

        (mainPathId, placeMainPath) <- placeLocationCard Locations.mainPath

        arkhamWoods <-
          genCards
            [ Locations.arkhamWoodsUnhallowedGround
            , Locations.arkhamWoodsTwistingPaths
            , Locations.arkhamWoodsOldHouse
            , Locations.arkhamWoodsCliffside
            , Locations.arkhamWoodsTangledThicket
            , Locations.arkhamWoodsQuietGlade
            , Locations.arkhamWoodsGreatWillow
            , Locations.arkhamWoodsLakeside
            , Locations.arkhamWoodsCorpseRiddenClearing
            , Locations.arkhamWoodsWoodenBridge
            ]

        woodsLocations <- take 4 <$> shuffleM arkhamWoods

        randomSet <-
          sample
            $ EncounterSet.AgentsOfYogSothoth
            :| [ EncounterSet.AgentsOfShubNiggurath
               , EncounterSet.AgentsOfCthulhu
               , EncounterSet.AgentsOfHastur
               ]

        encounterDeck <-
          buildEncounterDeckExcluding
            [Enemies.umordhoth]
            [ EncounterSet.ReturnToTheDevourerBelow
            , EncounterSet.TheDevourerBelow
            , EncounterSet.AncientEvils
            , EncounterSet.StrikingFear
            , EncounterSet.GhoulsOfUmordhoth
            , EncounterSet.TheDevourersCult
            , randomSet
            ]

        placeWoods <-
          for (zip woodsLabels woodsLocations) $ \(label, location) -> do
            (locationId, placement) <- placeLocation location
            pure [placement, SetLocationLabel locationId label]

        pushAll
          $ [ story investigatorIds intro
            , SetEncounterDeck encounterDeck
            , AddChaosToken ElderThing
            , SetAgendaDeck
            , SetActDeck
            , placeMainPath
            ]
          <> concat placeWoods
          <> [ RevealLocation Nothing mainPathId
             , MoveAllTo (toSource attrs) mainPathId
             ]
          <> ghoulPriestMessages
          <> cultistsWhoGotAwayMessages
          <> pastMidnightMessages

        setAsideEncounterCards <-
          genCards
            [Locations.ritualSite, Enemies.umordhoth]

        agendas <- genCards agendaDeck
        acts <- genCards actDeck

        ReturnToTheDevourerBelow
          . TheDevourerBelow
          <$> runMessage
            msg
            ( attrs
                & (setAsideCardsL .~ setAsideEncounterCards)
                & (actStackL . at 1 ?~ acts)
                & (agendaStackL . at 1 ?~ agendas)
            )
      CreateEnemy creation@(enemyCreationMethod -> SpawnAtLocation lid) | toCardCode (enemyCreationCard creation) == "01157" -> do
        name <- field LocationName lid
        when (name == "Ritual Site") $ do
          vaultOfEarthlyDemise <- genCard Treacheries.vaultOfEarthlyDemise
          push
            $ AttachStoryTreacheryTo
              vaultOfEarthlyDemise
              (toTarget $ enemyCreationEnemyId creation)
        pure s
      _ -> ReturnToTheDevourerBelow <$> runMessage msg theDevourerBelow'
