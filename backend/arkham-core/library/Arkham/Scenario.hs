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
  , ("01120", SomeScenario theMidnightMasks)
  , ("01142", SomeScenario theDevourerBelow)
  , ("02041", SomeScenario extracurricularActivity)
  , ("02062", SomeScenario theHouseAlwaysWins)
  , ("02118", SomeScenario theMiskatonicMuseum)
  , ("02159", SomeScenario theEssexCountyExpress)
  , ("02195", SomeScenario bloodOnTheAltar)
  , ("02236", SomeScenario undimensionedAndUnseen)
  , ("02274", SomeScenario whereDoomAwaits)
  , ("02311", SomeScenario lostInTimeAndSpace)
  , ("03043", SomeScenario curtainCall)
  , ("03061", SomeScenario theLastKing)
  , ("03120", SomeScenario echoesOfThePast)
  , ("03159", SomeScenario theUnspeakableOath)
  , ("03200", SomeScenario aPhantomOfTruth)
  , ("03240", SomeScenario thePallidMask)
  , ("03274", SomeScenario blackStarsRise)
  , ("03316", SomeScenario dimCarcosa)
  , ("04043", SomeScenario theUntamedWilds)
  , ("04054", SomeScenario theDoomOfEztli)
  , ("04113", SomeScenario threadsOfFate)
  , ("50011", SomeScenario returnToTheGathering)
  , ("50025", SomeScenario returnToTheMidnightMasks)
  , ("50032", SomeScenario returnToTheDevourerBelow)
  , ("81001", SomeScenario curseOfTheRougarou)
  , ("82001", SomeScenario carnevaleOfHorrors)
  ]
