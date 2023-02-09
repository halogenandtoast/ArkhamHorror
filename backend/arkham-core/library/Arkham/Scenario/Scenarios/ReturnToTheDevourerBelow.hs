module Arkham.Scenario.Scenarios.ReturnToTheDevourerBelow where

import Arkham.Prelude

import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types ( Field (..) )
import Arkham.Matcher (CardMatcher(..))
import Arkham.Message
import Arkham.Projection
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenario.Scenarios.TheDevourerBelow
import Arkham.Scenarios.TheDevourerBelow.Story
import Arkham.Target
import Arkham.Token
import Arkham.Treachery.Cards qualified as Treacheries

newtype ReturnToTheDevourerBelow = ReturnToTheDevourerBelow TheDevourerBelow
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

returnToTheDevourerBelow :: Difficulty -> ReturnToTheDevourerBelow
returnToTheDevourerBelow difficulty = scenario
  (ReturnToTheDevourerBelow . TheDevourerBelow)
  "01142"
  "The Devourer Below"
  difficulty
  [ "woods1     .     woods2"
  , "woods1 mainPath woods2"
  , "woods3 mainPath woods4"
  , "woods3 ritualSite woods4"
  , "   .   ritualSite   .  "
  ]

instance HasTokenValue ReturnToTheDevourerBelow where
  getTokenValue iid tokenFace (ReturnToTheDevourerBelow theDevourerBelow') =
    getTokenValue iid tokenFace theDevourerBelow'

instance RunMessage ReturnToTheDevourerBelow where
  runMessage msg s@(ReturnToTheDevourerBelow theDevourerBelow'@(TheDevourerBelow attrs))
    = case msg of
      Setup -> do
        investigatorIds <- allInvestigatorIds
        pastMidnight <- getHasRecord ItIsPastMidnight
        ghoulPriestAlive <- getHasRecord GhoulPriestIsStillAlive
        cultistsWhoGotAway <- getRecordSet CultistsWhoGotAway
        ghoulPriestCard <- genEncounterCard Enemies.ghoulPriest

        let
          toLocationCard = fmap EncounterCard . genEncounterCard
          woodsLabels = ["woods1", "woods2", "woods3", "woods4"]
          ghoulPriestMessages =
            [ AddToEncounterDeck ghoulPriestCard | ghoulPriestAlive ]
          pastMidnightMessages =
            if pastMidnight then [AllRandomDiscard (toSource attrs) AnyCard, AllRandomDiscard (toSource attrs) AnyCard] else []
          cultistsWhoGotAwayMessages = replicate
            ((length cultistsWhoGotAway + 1) `div` 2)
            PlaceDoomOnAgenda

        mainPath <- toLocationCard Locations.mainPath
        let mainPathId = LocationId $ toCardId mainPath

        arkhamWoods <- traverse
          toLocationCard
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

        encounterDeck <- buildEncounterDeckExcluding
          [Enemies.umordhoth]
          [ EncounterSet.ReturnToTheDevourerBelow
          , EncounterSet.TheDevourerBelow
          , EncounterSet.AncientEvils
          , EncounterSet.StrikingFear
          , EncounterSet.GhoulsOfUmordhoth
          , EncounterSet.TheDevourersCult
          , randomSet
          ]

        pushAllEnd
          $ [ story investigatorIds intro
            , SetEncounterDeck encounterDeck
            , AddToken ElderThing
            , SetAgendaDeck
            , SetActDeck
            , PlaceLocation mainPath
            ]
          <> [ PlaceLocation card | card <- woodsLocations ]
          <> [ SetLocationLabel (LocationId $ toCardId location) label
             | (label, location) <- zip woodsLabels woodsLocations
             ]
          <> [ RevealLocation Nothing mainPathId
             , MoveAllTo (toSource attrs) mainPathId
             ]
          <> ghoulPriestMessages
          <> cultistsWhoGotAwayMessages
          <> pastMidnightMessages

        setAsideEncounterCards <- traverse
          (fmap EncounterCard . genEncounterCard)
          [Locations.ritualSite, Enemies.umordhoth]

        ReturnToTheDevourerBelow . TheDevourerBelow <$> runMessage
          msg
          (attrs
          & (setAsideCardsL .~ setAsideEncounterCards)
          & (actStackL . at 1 ?~ actDeck)
          & (agendaStackL . at 1 ?~ agendaDeck)
          )
      CreateEnemyAt card lid _ | toCardCode card == "01157" -> do
        name <- field LocationName lid
        if name == "Ritual Site"
          then do
            vaultOfEarthlyDemise <- EncounterCard
              <$> genEncounterCard Treacheries.vaultOfEarthlyDemise
            push $ AttachStoryTreacheryTo
              vaultOfEarthlyDemise
              (CardCodeTarget "00157")
            pure s
          else pure s
      _ -> ReturnToTheDevourerBelow <$> runMessage msg theDevourerBelow'
