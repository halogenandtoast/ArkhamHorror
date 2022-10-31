{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Scenario
  ( module Arkham.Scenario
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Message
import Arkham.Scenario.Runner
import Arkham.Scenario.Scenarios
import Arkham.Target
import Arkham.Token

instance FromJSON Scenario where
  parseJSON v = flip (withObject "Scenario") v $ \o -> do
    cCode :: CardCode <- o .: "id"
    case lookup cCode allScenarios of
      Nothing -> error $ "Unknown scenario: " <> show cCode
      Just (SomeScenario (_ :: Difficulty -> a)) -> Scenario <$> parseJSON @a v

instance RunMessage Scenario where
  runMessage msg x@(Scenario s) = case msg of
    ResolveToken _ tokenFace _ -> do
      modifiers' <- getModifiers (TokenFaceTarget tokenFace)
      if any (`elem` modifiers') [IgnoreTokenEffects, IgnoreToken]
        then pure x
        else go
    FailedSkillTest _ _ _ (TokenTarget token) _ _ -> do
      modifiers' <- getModifiers (TokenFaceTarget $ tokenFace token)
      if any (`elem` modifiers') [IgnoreTokenEffects, IgnoreToken]
        then pure x
        else go
    PassedSkillTest _ _ _ (TokenTarget token) _ _ -> do
      modifiers' <- getModifiers (TokenFaceTarget $ tokenFace token)
      if any (`elem` modifiers') [IgnoreTokenEffects, IgnoreToken]
        then pure x
        else go
    _ -> go
    where go = Scenario <$> runMessage msg s

instance HasTokenValue Scenario where
  getTokenValue iid tokenFace (Scenario s) = do
    modifiers' <- getModifiers (TokenFaceTarget tokenFace)
    if any (`elem` modifiers') [IgnoreTokenEffects, IgnoreToken]
      then pure $ TokenValue tokenFace NoModifier
      else getTokenValue iid tokenFace s

lookupScenario :: ScenarioId -> Difficulty -> Scenario
lookupScenario scenarioId =
  case lookup (unScenarioId scenarioId) allScenarios of
    Nothing -> error $ "Unknown scenario: " <> show scenarioId
    Just (SomeScenario f) -> Scenario . f

data SomeScenario = forall a . IsScenario a => SomeScenario (Difficulty -> a)

allScenarios :: HashMap CardCode SomeScenario
allScenarios = mapFromList
  [ ("01104", SomeScenario theGathering)
  ]
