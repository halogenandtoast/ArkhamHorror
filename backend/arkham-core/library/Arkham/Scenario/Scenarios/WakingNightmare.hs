module Arkham.Scenario.Scenarios.WakingNightmare (
  WakingNightmare (..),
  wakingNightmare,
) where

import Arkham.Prelude

import Arkham.CampaignLogKey
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Scenario.Runner
import Arkham.Scenarios.WakingNightmare.FlavorText

newtype WakingNightmare = WakingNightmare ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wakingNightmare :: Difficulty -> WakingNightmare
wakingNightmare difficulty =
  scenario
    WakingNightmare
    "06063"
    "Waking Nightmare"
    difficulty
    [ ".             recordsOffice ."
    , ".             waitingRoom   ."
    , "emergencyRoom .             experimentalTherapiesWard"
    ]

instance HasChaosTokenValue WakingNightmare where
  getChaosTokenValue iid tokenFace (WakingNightmare attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage WakingNightmare where
  runMessage msg s@(WakingNightmare attrs) = case msg of
    PreScenarioSetup -> do
      players <- allPlayers
      lead <- getLeadPlayer
      push
        $ storyWithChooseOne
          lead
          players
          intro1
          [ Label
              "Convince Doctor Maheswaran to come with you while you investigate, for her safety and yours."
              [story players intro2, Record DrMaheswaranJoinedTheInvestigation]
          , Label
              "Convince Doctor Maheswaran to stay with the patients and keep them safe while you investigate."
              [story players intro3, Record DrMaheswaranStayedWithHerPatients]
          ]
      pure s
    Setup -> do
      push $ EndOfGame Nothing
      pure s
    _ -> WakingNightmare <$> runMessage msg attrs
