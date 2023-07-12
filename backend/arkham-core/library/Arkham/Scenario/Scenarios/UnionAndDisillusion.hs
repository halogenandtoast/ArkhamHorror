module Arkham.Scenario.Scenarios.UnionAndDisillusion (
  UnionAndDisillusion (..),
  unionAndDisillusion,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Message
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.UnionAndDisillusion.Story
import Arkham.Token
import Arkham.Treachery.Cards qualified as Treacheries

newtype UnionAndDisillusion = UnionAndDisillusion ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unionAndDisillusion :: Difficulty -> UnionAndDisillusion
unionAndDisillusion difficulty =
  scenario
    UnionAndDisillusion
    "05238"
    "Union and Disillusion"
    difficulty
    [ ".              miskatonicRiver ."
    , ".              forbiddingShore ."
    , "unvisitedIsle1 .               unvisitedIsle2"
    ]

instance HasTokenValue UnionAndDisillusion where
  getTokenValue iid tokenFace (UnionAndDisillusion attrs) = case tokenFace of
    Skull -> pure $ toTokenValue attrs Skull 3 5
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ TokenValue Tablet NoModifier
    ElderThing -> pure $ TokenValue ElderThing NoModifier
    otherFace -> getTokenValue iid otherFace attrs

instance RunMessage UnionAndDisillusion where
  runMessage msg s@(UnionAndDisillusion attrs) = case msg of
    PreScenarioSetup -> do
      investigators <- allInvestigators
      lead <- getLead
      pushAll
        [ story investigators intro
        , questionLabel
            "This is a point of no return—you will not get the chance to change your mind later. The investigators must decide (choose one):"
            lead
            $ ChooseOne
              [ Label
                  "\"We have to help complete the Lodge’s ritual.\" Completing the ritual should bind the Spectral Watcher and prevent it from doing any more harm."
                  [Record TheInvestigatorsSidedWithTheLodge]
              , Label
                  "\"We have to stop the Lodge’s ritual.\" Disrupting the ritual should release the Spectral Watcher’s tether to the mortal realm."
                  [Record TheInvestigatorsSidedWithTheCoven]
              ]
        ]
      pure s
    Setup -> do
      encounterDeck <-
        buildEncounterDeckExcluding
          [Treacheries.watchersGaze]
          [ EncounterSet.UnionAndDisillusion
          , EncounterSet.InexorableFate
          , EncounterSet.RealmOfDeath
          , EncounterSet.SpectralPredators
          , EncounterSet.AncientEvils
          , EncounterSet.ChillingCold
          ]
      setAsideCards <-
        concatMapM
          (fmap (map toCard) . gatherEncounterSet)
          [EncounterSet.AnettesCoven, EncounterSet.SilverTwilightLodge, EncounterSet.TheWatcher]

      sidedWithTheLodge <- getHasRecord TheInvestigatorsSidedWithTheLodge
      sidedWithTheCoven <- getHasRecord TheInvestigatorsSidedWithTheCoven
      deceivingTheLodge <- getHasRecord TheInvestigatorsAreDeceivingTheLodge
      inductedIntoTheInnerCircle <- getHasRecord TheInvestigatorsWereInductedIntoTheInnerCircle
      hidTheirKnowledge <- getHasRecord TheInvestigatorsHidTheirKnowledgeOfTheCoven
      keptMementosHidden <- getHasRecord TheInvestigatorsKeptsTheirMementosHidden
      hereticCount <- getRecordCount HereticsWereUnleashedUntoArkham

      unvisitedIsles <-
        sampleN
          2
          $ Locations.unvisitedIsleStandingStones
            :| [ Locations.unvisitedIsleMistyClearing
               , Locations.unvisitedIsleForsakenWoods
               , Locations.unvisitedIsleMossCoveredSteps
               , Locations.unvisitedIsleHauntedSpring
               , Locations.unvisitedIsleDecayedWillow
               ]

      (miskatonicRiverId, placeMiskatonicRiver) <- placeLocationCard Locations.miskatonicRiver
      placeForbiddingShore <- placeLocationCard_ Locations.forbiddingShore
      placeUnvisitedIsles <- placeLabeledLocations "unvisitedIsle" unvisitedIsles

      pushAll $
        [SetEncounterDeck encounterDeck, SetAgendaDeck, SetActDeck]
          <> replicate hereticCount PlaceDoomOnAgenda
          <> [placeMiskatonicRiver, MoveAllTo (toSource attrs) miskatonicRiverId, placeForbiddingShore]
          <> placeUnvisitedIsles

      let
        (act3, act4)
          | sidedWithTheLodge = (Acts.beyondTheMistV1, Acts.theBindingRite)
          | sidedWithTheCoven && deceivingTheLodge && inductedIntoTheInnerCircle =
              (Acts.beyondTheMistV2, Acts.theBrokenRite)
          | sidedWithTheCoven && count id [deceivingTheLodge, hidTheirKnowledge, keptMementosHidden] >= 2 =
              (Acts.beyondTheMistV3, Acts.theBrokenRite)
          | otherwise = (Acts.beyondTheMistV4, Acts.theBrokenRite)

      agendas <- genCards [Agendas.theLoversVI, Agendas.crossroadsOfFate]
      acts <- genCards [Acts.theUnvisitedIsle, Acts.fatedSouls, act3, act4]

      UnionAndDisillusion
        <$> runMessage
          msg
          ( attrs
              & (setAsideCardsL .~ setAsideCards)
              & (agendaStackL . at 1 ?~ agendas)
              & (actStackL . at 1 ?~ acts)
          )
    _ -> UnionAndDisillusion <$> runMessage msg attrs
