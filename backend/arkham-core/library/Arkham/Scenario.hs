{-# LANGUAGE TemplateHaskell #-}

module Arkham.Scenario
  ( module Arkham.Scenario
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Decks
import Arkham.Difficulty
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Query
import Arkham.Scenario.Attrs
import Arkham.Scenario.Runner
import Arkham.Scenario.Scenarios
import Arkham.ScenarioLogKey
import Arkham.Target
import Arkham.Token
import Arkham.Trait (Trait)

$(buildEntity "Scenario")

instance HasRecord env Scenario

instance
  ( HasSet ClosestAssetId env (InvestigatorId, AssetMatcher)
  , HasStep ActStep env ()
  , ScenarioRunner env
  )
  => RunMessage env Scenario where
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
   where
     go = $(entityRunMessage "Scenario")

instance
  ( HasCount DiscardCount env InvestigatorId
  , HasCount DoomCount env ()
  , HasCount DoomCount env EnemyId
  , HasCount Shroud env LocationId
  , Query EnemyMatcher env
  , HasSet EnemyId env Trait
  , HasSet EnemyId env LocationId
  , HasSet LocationId env [Trait]
  , HasSet LocationId env ()
  , HasSet Trait env LocationId
  , HasList UnderneathCard env LocationId
  , HasTokenValue env InvestigatorId
  , HasId LocationId env InvestigatorId
  , HasId CardCode env EnemyId
  , HasStep AgendaStep env ()
  , HasCount HorrorCount env InvestigatorId
  , HasModifiersFor env ()
  )
  => HasTokenValue env Scenario where
  getTokenValue s iid tokenFace = do
    modifiers' <- getModifiers
      (toSource $ toAttrs s)
      (TokenFaceTarget tokenFace)
    if any (`elem` modifiers') [IgnoreTokenEffects, IgnoreToken]
      then pure $ TokenValue tokenFace NoModifier
      else defaultGetTokenValue s iid tokenFace

instance Entity Scenario where
  type EntityId Scenario = ScenarioId
  type EntityAttrs Scenario = ScenarioAttrs

instance HasSet ScenarioLogKey env Scenario where
  getSet = pure . scenarioLog . toAttrs

instance HasCount ScenarioDeckCount env (Scenario, ScenarioDeckKey) where
  getCount (scenario, key) = getCount (toAttrs scenario, key)

instance HasCount SetAsideCount env (Scenario, CardCode) where
  getCount = getCount . first toAttrs

instance HasList SetAsideCard env Scenario where
  getList = getList . toAttrs

instance HasList UnderScenarioReferenceCard env Scenario where
  getList = getList . toAttrs

instance HasList UnderneathCard env (Scenario, ActDeck) where
  getList (s, _) = getList (toAttrs s, ActDeck)

instance HasList UnderneathCard env (Scenario, AgendaDeck) where
  getList (s, _) = getList (toAttrs s, AgendaDeck)

instance HasName env Scenario where
  getName = getName . toAttrs

instance HasCampaignStoryCard env Scenario where
  getCampaignStoryCard def s = pure . fromJustNote "missing card" $ find
    ((== def) . toCardDef)
    cards
   where
    attrs = toAttrs s
    cards = concat . toList $ scenarioStoryCards attrs

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
  , ("50011", ReturnToTheGathering' . returnToTheGathering)
  , ("50025", ReturnToTheMidnightMasks' . returnToTheMidnightMasks)
  , ("50032", ReturnToTheDevourerBelow' . returnToTheDevourerBelow)
  , ("81001", CurseOfTheRougarou' . curseOfTheRougarou)
  , ("82001", CarnevaleOfHorrors' . carnevaleOfHorrors)
  ]
