module Arkham.Scenario.Scenarios.ThreadsOfFate
  ( ThreadsOfFate(..)
  , threadsOfFate
  ) where

import Arkham.Prelude

import Arkham.CampaignLogKey
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Message
import Arkham.Scenario.Runner
import Arkham.ScenarioLogKey
import Arkham.Scenarios.ThreadsOfFate.Story
import Arkham.Token

newtype ThreadsOfFate = ThreadsOfFate ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

threadsOfFate :: Difficulty -> ThreadsOfFate
threadsOfFate difficulty = scenarioWith
  ThreadsOfFate
  "04113"
  "Threads of Fate"
  difficulty
  [ "curiositieShoppe curiositieShoppe northside northside            downtown             downtown  easttown  easttown velmasDiner velmasDiner"
  , ".                .                .         miskatonicUniversity miskatonicUniversity rivertown rivertown .        .           ."
  ]
  (decksLayoutL .~ [". act1", "agenda1 act2", ". act3"])


instance HasTokenValue ThreadsOfFate where
  getTokenValue iid tokenFace (ThreadsOfFate attrs) = case tokenFace of
    Skull -> pure $ toTokenValue attrs Skull 3 5
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ TokenValue Tablet NoModifier
    ElderThing -> pure $ TokenValue ElderThing NoModifier
    otherFace -> getTokenValue iid otherFace attrs

instance RunMessage ThreadsOfFate where
  runMessage msg s@(ThreadsOfFate attrs) = case msg of
    Setup -> do
      iids <- getInvestigatorIds
      leadInvestigatorId <- getLeadInvestigatorId
      gaveCustodyToHarlan <- getHasRecord
        TheInvestigatorsGaveCustodyOfTheRelicToHarlanEarnstone
      let intro2or3 = if gaveCustodyToHarlan then intro3 else intro2
      pushAll
        [ story iids intro1
        , story iids intro2or3
        , chooseOne
          leadInvestigatorId
          [ Label
            "“You’re not going anywhere until you tell me what is going on.” - Skip to Intro 4."
            [SetupStep (toTarget attrs) 4]
          , Label
            "“Have it your way.” - Skip to Intro 5."
            [SetupStep (toTarget attrs) 5]
          ]
        ]
      pure s
    SetupStep target n | isTarget attrs target -> do
      (msgs, mNextStep) <- case n of
        4 ->
          pure ([Remember YouListenedToIchtacasTale, AddToken Cultist], Nothing)
        5 -> do
          gaveCustodyToHarlan <- getHasRecord
            TheInvestigatorsGaveCustodyOfTheRelicToHarlanEarnstone
          pure
            ( [Remember IchtacaLeftWithoutYou]
            , if gaveCustodyToHarlan then Just 6 else Nothing
            )
        6 -> pure
          ( [ RemoveAllTokens Cultist
            , RemoveAllTokens Tablet
            , AddToken ElderThing
            , Record YouAreForgingYourOwnWay
            ]
          , Nothing
          )
        _ -> error "Invalid step"
      case mNextStep of
        Just nextStep -> do
          pushAll $ msgs <> [SetupStep (toTarget attrs) nextStep]
          pure s
        Nothing -> do
          pushAll msgs
          pure s
    _ -> ThreadsOfFate <$> runMessage msg attrs
