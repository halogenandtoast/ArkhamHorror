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
import Data.Typeable

data Scenario = forall a. IsScenario a => Scenario a

instance Eq Scenario where
  (Scenario (a :: a)) == (Scenario (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Scenario where
  show (Scenario a) = show a

instance ToJSON Scenario where
  toJSON (Scenario a) = toJSON a

instance FromJSON Scenario where
  parseJSON v = flip (withObject "Scenario") v $ \o -> do
    cCode :: CardCode <- o .: "id"
    case cCode of
      "01104" -> Scenario . TheGathering <$> parseJSON v
      "01120" -> Scenario . TheMidnightMasks <$> parseJSON v
      "01142" -> Scenario . TheDevourerBelow <$> parseJSON v
      "02041" -> Scenario . ExtracurricularActivity <$> parseJSON v
      "02062" -> Scenario . TheHouseAlwaysWins <$> parseJSON v
      "02118" -> Scenario . TheMiskatonicMuseum <$> parseJSON v
      "02159" -> Scenario . TheEssexCountyExpress <$> parseJSON v
      "02195" -> Scenario . BloodOnTheAltar <$> parseJSON v
      "02236" -> Scenario . UndimensionedAndUnseen <$> parseJSON v
      "02274" -> Scenario . WhereDoomAwaits <$> parseJSON v
      "02311" -> Scenario . LostInTimeAndSpace <$> parseJSON v
      "03043" -> Scenario . CurtainCall <$> parseJSON v
      "03061" -> Scenario . TheLastKing <$> parseJSON v
      "03120" -> Scenario . EchoesOfThePast <$> parseJSON v
      "03159" -> Scenario . TheUnspeakableOath <$> parseJSON v
      "03200" -> Scenario . APhantomOfTruth <$> parseJSON v
      "03240" -> Scenario . ThePallidMask <$> parseJSON v
      "03274" -> Scenario . BlackStarsRise <$> parseJSON v
      "03316" -> Scenario . DimCarcosa <$> parseJSON v
      "50011" -> Scenario . ReturnToTheGathering <$> parseJSON v
      "50025" -> Scenario . ReturnToTheMidnightMasks <$> parseJSON v
      "50032" -> Scenario . ReturnToTheDevourerBelow <$> parseJSON v
      "81001" -> Scenario . CurseOfTheRougarou <$> parseJSON v
      "82001" -> Scenario . CarnevaleOfHorrors <$> parseJSON v
      _ -> error "Unknown scenario"

instance HasModifiersFor Scenario where
  getModifiersFor source target (Scenario a) = getModifiersFor source target a

instance RunMessage Scenario where
  runMessage msg x@(Scenario s) = case msg of
    ResolveToken _ tokenFace _ -> do
      modifiers' <- getModifiers
        (toSource $ toAttrs s)
        (TokenFaceTarget tokenFace)
      if any (`elem` modifiers') [IgnoreTokenEffects, IgnoreToken]
        then pure x
        else go
    FailedSkillTest _ _ _ (TokenTarget token) _ _ -> do
      modifiers' <- getModifiers
        (toSource $ toAttrs s)
        (TokenFaceTarget $ tokenFace token)
      if any (`elem` modifiers') [IgnoreTokenEffects, IgnoreToken]
        then pure x
        else go
    PassedSkillTest _ _ _ (TokenTarget token) _ _ -> do
      modifiers' <- getModifiers
        (toSource $ toAttrs s)
        (TokenFaceTarget $ tokenFace token)
      if any (`elem` modifiers') [IgnoreTokenEffects, IgnoreToken]
        then pure x
        else go
    _ -> go
    where go = Scenario <$> runMessage msg s

instance HasTokenValue Scenario where
  getTokenValue iid tokenFace (Scenario s) = do
    modifiers' <- getModifiers
      (toSource $ toAttrs s)
      (TokenFaceTarget tokenFace)
    if any (`elem` modifiers') [IgnoreTokenEffects, IgnoreToken]
      then pure $ TokenValue tokenFace NoModifier
      else getTokenValue iid tokenFace s

instance Entity Scenario where
  type EntityId Scenario = ScenarioId
  type EntityAttrs Scenario = ScenarioAttrs
  toId = toId . toAttrs
  toAttrs (Scenario a) = toAttrs a
  overAttrs f (Scenario a) = Scenario $ overAttrs f a

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
  [ ("01104", Scenario . theGathering)
  , ("01120", Scenario . theMidnightMasks)
  , ("01142", Scenario . theDevourerBelow)
  , ("02041", Scenario . extracurricularActivity)
  , ("02062", Scenario . theHouseAlwaysWins)
  , ("02118", Scenario . theMiskatonicMuseum)
  , ("02159", Scenario . theEssexCountyExpress)
  , ("02195", Scenario . bloodOnTheAltar)
  , ("02236", Scenario . undimensionedAndUnseen)
  , ("02274", Scenario . whereDoomAwaits)
  , ("02311", Scenario . lostInTimeAndSpace)
  , ("03043", Scenario . curtainCall)
  , ("03061", Scenario . theLastKing)
  , ("03120", Scenario . echoesOfThePast)
  , ("03159", Scenario . theUnspeakableOath)
  , ("03200", Scenario . aPhantomOfTruth)
  , ("03240", Scenario . thePallidMask)
  , ("03274", Scenario . blackStarsRise)
  , ("03316", Scenario . dimCarcosa)
  , ("50011", Scenario . returnToTheGathering)
  , ("50025", Scenario . returnToTheMidnightMasks)
  , ("50032", Scenario . returnToTheDevourerBelow)
  , ("81001", Scenario . curseOfTheRougarou)
  , ("82001", Scenario . carnevaleOfHorrors)
  ]
