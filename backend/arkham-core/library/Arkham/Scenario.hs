{-# LANGUAGE TemplateHaskell #-}

module Arkham.Scenario
  ( module Arkham.Scenario
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Classes.Entity.TH
import Arkham.Difficulty
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Message
import Arkham.Scenario.Runner
import Arkham.Scenario.Scenarios
import Arkham.Target
import Arkham.Token
import Data.Aeson.TH

$(buildEntity "Scenario")

$(deriveJSON defaultOptions ''Scenario)

instance HasModifiersFor Scenario where
  getModifiersFor = $(entityF2 "Scenario" "getModifiersFor")

instance RunMessage Scenario where
  runMessage msg s = case msg of
    ResolveToken _ tokenFace _ -> do
      modifiers' <- getModifiers
        (toSource $ toAttrs s)
        (TokenFaceTarget tokenFace)
      if any (`elem` modifiers') [IgnoreTokenEffects, IgnoreToken]
        then pure s
        else go msg s
    FailedSkillTest _ _ _ (TokenTarget token) _ _ -> do
      modifiers' <- getModifiers
        (toSource $ toAttrs s)
        (TokenFaceTarget $ tokenFace token)
      if any (`elem` modifiers') [IgnoreTokenEffects, IgnoreToken]
        then pure s
        else go msg s
    PassedSkillTest _ _ _ (TokenTarget token) _ _ -> do
      modifiers' <- getModifiers
        (toSource $ toAttrs s)
        (TokenFaceTarget $ tokenFace token)
      if any (`elem` modifiers') [IgnoreTokenEffects, IgnoreToken]
        then pure s
        else go msg s
    _ -> go msg s
    where go = $(entityRunMessage "Scenario")

instance HasTokenValue Scenario where
  getTokenValue iid tokenFace s = do
    modifiers' <- getModifiers
      (toSource $ toAttrs s)
      (TokenFaceTarget tokenFace)
    if any (`elem` modifiers') [IgnoreTokenEffects, IgnoreToken]
      then pure $ TokenValue tokenFace NoModifier
      else $(entityF2 "Scenario" "getTokenValue") iid tokenFace s

instance Entity Scenario where
  type EntityId Scenario = ScenarioId
  type EntityAttrs Scenario = ScenarioAttrs
  toId = toId . toAttrs
  toAttrs = $(entityF "Scenario" "toAttrs")

lookupScenario :: ScenarioId -> Difficulty -> Scenario
lookupScenario = fromJustNote "Unknown scenario" . flip lookup allScenarios

difficultyOfScenario :: Scenario -> Difficulty
difficultyOfScenario = scenarioDifficulty . toAttrs

scenarioActs :: Scenario -> [CardDef]
scenarioActs s = case mapToList $ scenarioActStack (toAttrs s) of
  [(_, actIds)] -> actIds
  _ -> error "Not able to handle multiple act stacks yet"

allScenarios :: HashMap ScenarioId (Difficulty -> Scenario)
allScenarios = mapFromList
  [ ("01104", TheGathering' . theGathering)
  , ("01120", TheMidnightMasks' . theMidnightMasks)
  , ("01142", TheDevourerBelow' . theDevourerBelow)
  , ("02041", ExtracurricularActivity' . extracurricularActivity)
  , ("02062", TheHouseAlwaysWins' . theHouseAlwaysWins)
  , ("02118", TheMiskatonicMuseum' . theMiskatonicMuseum)
  , ("02159", TheEssexCountyExpress' . theEssexCountyExpress)
  , ("02195", BloodOnTheAltar' . bloodOnTheAltar)
  , ("02236", UndimensionedAndUnseen' . undimensionedAndUnseen)
  , ("02274", WhereDoomAwaits' . whereDoomAwaits)
  , ("02311", LostInTimeAndSpace' . lostInTimeAndSpace)
  , ("03043", CurtainCall' . curtainCall)
  , ("03061", TheLastKing' . theLastKing)
  , ("03120", EchoesOfThePast' . echoesOfThePast)
  , ("03159", TheUnspeakableOath' . theUnspeakableOath)
  , ("03200", APhantomOfTruth' . aPhantomOfTruth)
  , ("03240", ThePallidMask' . thePallidMask)
  , ("03274", BlackStarsRise' . blackStarsRise)
  , ("50011", ReturnToTheGathering' . returnToTheGathering)
  , ("50025", ReturnToTheMidnightMasks' . returnToTheMidnightMasks)
  , ("50032", ReturnToTheDevourerBelow' . returnToTheDevourerBelow)
  , ("81001", CurseOfTheRougarou' . curseOfTheRougarou)
  , ("82001", CarnevaleOfHorrors' . carnevaleOfHorrors)
  ]
