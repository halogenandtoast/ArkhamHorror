module Arkham.Scenario.Scenarios.TheDoomOfEztli
  ( TheDoomOfEztli(..)
  , theDoomOfEztli
  ) where

import Arkham.Prelude

import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheDoomOfEztli.Story
import Arkham.Token

newtype TheDoomOfEztli = TheDoomOfEztli ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDoomOfEztli :: Difficulty -> TheDoomOfEztli
theDoomOfEztli difficulty = scenario
  TheDoomOfEztli
  "04054"
  "The Doom of Eztli"
  difficulty
  [ ".        ancientHall  undergroundRuins .             ."
  , "entryway ancientHall  undergroundRuins secretPassage chamberOfTime"
  , "entryway grandChamber burialPit        secretPassage chamberOfTime"
  , ".        grandChamber burialPit        .             ."
  ]

instance HasTokenValue TheDoomOfEztli where
  getTokenValue iid tokenFace (TheDoomOfEztli attrs) = case tokenFace of
    Skull -> do
      hasDoom <- selectAny $ LocationWithAnyDoom <> locationWithInvestigator iid
      pure $ if hasDoom
        then toTokenValue attrs Skull 3 4
        else toTokenValue attrs Skull 1 2
    face | face `elem` [Cultist, Tablet] -> do
      n <- if isEasyStandard attrs
        then selectCount LocationWithAnyDoom
        else getSum <$> selectAgg Sum LocationDoom LocationWithAnyDoom
      pure $ TokenValue Cultist (NegativeModifier n)
    ElderThing -> pure $ TokenValue ElderThing NoModifier
    otherFace -> getTokenValue iid otherFace attrs

instance RunMessage TheDoomOfEztli where
  runMessage msg s@(TheDoomOfEztli attrs) = case msg of
    Setup -> do
      iids <- getInvestigatorIds
      -- | Determine intro
      forcedToWaitForAdditionalSupplies <- getHasRecord
        TheInvestigatorsWereForcedToWaitForAdditionalSupplies
      let intro = if forcedToWaitForAdditionalSupplies then intro1 else intro2
      -- | Setup
      -- -- | Gather cards
      encounterDeck <- buildEncounterDeck
        [ EncounterSet.TheDoomOfEztli
        , EncounterSet.AgentsOfYig
        , EncounterSet.YigsVenom
        , EncounterSet.TemporalFlux
        , EncounterSet.DeadlyTraps
        , EncounterSet.ForgottenRuins
        , EncounterSet.Poison
        , EncounterSet.ChillingCold
        ]
      -- Put entryway into play investigators start there
      entryway <- genCard Locations.entryway
      -- | Messages
      pushAll
        [ story iids intro
        , SetEncounterDeck encounterDeck
        , SetAgendaDeck
        , SetActDeck
        , PlaceLocation entryway
        , MoveAllTo (toSource attrs) (toLocationId entryway)
        ]
      pure s
    _ -> TheDoomOfEztli <$> runMessage msg attrs
