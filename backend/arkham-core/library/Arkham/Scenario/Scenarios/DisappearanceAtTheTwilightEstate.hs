module Arkham.Scenario.Scenarios.DisappearanceAtTheTwilightEstate
  ( DisappearanceAtTheTwilightEstate(..)
  , disappearanceAtTheTwilightEstate
  ) where

import Arkham.Prelude

import Arkham.Scenario.Helpers
import Arkham.Classes
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Act.Cards qualified as Acts
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Message
import Arkham.Scenario.Runner
import Arkham.Scenarios.DisappearanceAtTheTwilightEstate.Story
import Arkham.Token

newtype DisappearanceAtTheTwilightEstate = DisappearanceAtTheTwilightEstate ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disappearanceAtTheTwilightEstate
  :: Difficulty -> DisappearanceAtTheTwilightEstate
disappearanceAtTheTwilightEstate difficulty = scenario
  DisappearanceAtTheTwilightEstate
  "05043"
  "Disappearance at the Twilight Estate"
  difficulty
  [ ".             .          office         .             ."
  , "billiardsRoom trophyRoom victorianHalls masterBedroom balcony"
  , ".             .          entryHall      .             ."
  ]

instance HasTokenValue DisappearanceAtTheTwilightEstate where
  getTokenValue iid tokenFace (DisappearanceAtTheTwilightEstate attrs) =
    case tokenFace of
      Skull -> pure $ toTokenValue attrs Skull 3 5
      otherFace -> getTokenValue iid otherFace attrs

instance RunMessage DisappearanceAtTheTwilightEstate where
  runMessage msg s@(DisappearanceAtTheTwilightEstate attrs) = case msg of
    Setup -> do
      investigatorIds <- allInvestigatorIds
      encounterDeck <- buildEncounterDeck
        [ EncounterSet.DisappearanceAtTheTwilightEstate
        , EncounterSet.InexorableFate
        , EncounterSet.RealmOfDeath
        , EncounterSet.SpectralPredators
        , EncounterSet.TrappedSpirits
        , EncounterSet.TheWatcher
        , EncounterSet.ChillingCold
        ]
      pushAll [story investigatorIds prologue
              , SetEncounterDeck encounterDeck
              , SetAgendaDeck
              , SetActDeck
              ]
      DisappearanceAtTheTwilightEstate <$> runMessage
        msg
        (attrs
        & (actStackL . at 1 ?~ [Acts.theDisappearance])
        & (agendaStackL . at 1 ?~ [ Agendas.judgementXX ])
        )
    _ -> DisappearanceAtTheTwilightEstate <$> runMessage msg attrs
