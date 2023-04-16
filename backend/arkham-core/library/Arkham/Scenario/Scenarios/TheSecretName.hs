module Arkham.Scenario.Scenarios.TheSecretName
  ( TheSecretName(..)
  , theSecretName
  ) where

import Arkham.Prelude

import Arkham.CampaignLogKey
import Arkham.Classes
import Arkham.ClassSymbol
import Arkham.Difficulty
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheSecretName.Story
import Arkham.Token

newtype TheSecretName = TheSecretName ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSecretName :: Difficulty -> TheSecretName
theSecretName difficulty =
  scenario TheSecretName "05120" "The Secret Name" difficulty []

instance HasTokenValue TheSecretName where
  getTokenValue iid tokenFace (TheSecretName attrs) = case tokenFace of
    Skull -> pure $ toTokenValue attrs Skull 3 5
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ TokenValue Tablet NoModifier
    ElderThing -> pure $ TokenValue ElderThing NoModifier
    otherFace -> getTokenValue iid otherFace attrs

instance RunMessage TheSecretName where
  runMessage msg s@(TheSecretName attrs) = case msg of
    PreScenarioSetup -> do
      iids <- getInvestigatorIds
      lead <- getLead
      anyMystic <- selectAny $ InvestigatorWithClass Mystic
      membersOfTheLodge <- getHasRecord TheInvestigatorsAreMembersOfTheLodge
      enemiesOfTheLodge <- getHasRecord TheInvestigatorsAreEnemiesOfTheLodge
      learnedNothing <- getHasRecord
        TheInvestigatorsLearnedNothingOfTheLodge'sSchemes
      neverSeenOrHeardFromAgain <- getHasRecord
        TheInvestigatorsAreNeverSeenOrHeardFromAgain
      pushAll
        $ [ storyWithChooseOne
              lead
              iids
              intro1
              [ Label
                "Tell the Lodge of the witches in the woods."
                [ story iids intro2
                , Record TheInvestigatorsToldTheLodgeAboutTheCoven
                , AddToken Cultist
                ]
              , Label
                "Tell him you know of no possible connection. (You are lying.)"
                [ story
                  iids
                  (intro3
                  <> (if anyMystic then intro3Mystic else mempty)
                  <> intro3Part2
                  )
                , Record TheInvestigatorsHidTheirKnowledgeOfTheCoven
                ]
              ]
          | membersOfTheLodge
          ]
        <> [ story iids intro4 | enemiesOfTheLodge ]
        <> [ story iids intro5 | learnedNothing ]
        <> [ story iids intro6 | neverSeenOrHeardFromAgain ]
      pure s
    Setup -> do
      pure s
    _ -> TheSecretName <$> runMessage msg attrs
