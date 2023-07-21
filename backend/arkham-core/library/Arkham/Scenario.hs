{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Scenario (
  module Arkham.Scenario,
) where

import Arkham.Prelude

import Arkham.Asset.Uses
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet (EncounterSet)
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Message
import Arkham.Name
import Arkham.Scenario.Runner
import Arkham.Scenario.Scenarios

instance FromJSON Scenario where
  parseJSON = withObject "Scenario" $ \o -> do
    cCode <- o .: "id"
    case lookup cCode allScenarios of
      Nothing -> error $ "Unknown scenario: " <> show cCode
      Just (SomeScenario (_ :: Difficulty -> a)) ->
        Scenario <$> parseJSON @a (Object o)

instance RunMessage Scenario where
  runMessage msg x@(Scenario s) = case msg of
    ResolveChaosToken _ chaosTokenFace _ -> do
      modifiers' <- getModifiers (ChaosTokenFaceTarget chaosTokenFace)
      if any (`elem` modifiers') [IgnoreChaosTokenEffects, IgnoreChaosToken]
        then pure x
        else go
    FailedSkillTest _ _ _ (ChaosTokenTarget token) _ _ -> do
      modifiers' <- getModifiers (ChaosTokenFaceTarget $ chaosTokenFace token)
      if any (`elem` modifiers') [IgnoreChaosTokenEffects, IgnoreChaosToken]
        then pure x
        else go
    PassedSkillTest _ _ _ (ChaosTokenTarget token) _ _ -> do
      modifiers' <- getModifiers (ChaosTokenFaceTarget $ chaosTokenFace token)
      if any (`elem` modifiers') [IgnoreChaosTokenEffects, IgnoreChaosToken]
        then pure x
        else go
    _ -> go
   where
    go = Scenario <$> runMessage msg s

instance HasChaosTokenValue Scenario where
  getChaosTokenValue iid chaosTokenFace (Scenario s) = do
    modifiers' <- getModifiers (ChaosTokenFaceTarget chaosTokenFace)
    if any (`elem` modifiers') [IgnoreChaosTokenEffects, IgnoreChaosToken]
      then pure $ ChaosTokenValue chaosTokenFace NoModifier
      else getChaosTokenValue iid chaosTokenFace s

lookupScenario :: ScenarioId -> Difficulty -> Scenario
lookupScenario scenarioId =
  case lookup (unScenarioId scenarioId) allScenarios of
    Nothing -> error $ "Unknown scenario: " <> show scenarioId
    Just (SomeScenario f) -> Scenario . f

data SomeScenario = forall a. IsScenario a => SomeScenario (Difficulty -> a)

scenarioCard :: CardCode -> Name -> EncounterSet -> CardDef
scenarioCard cCode name ecSet =
  CardDef
    { cdCardCode = cCode
    , cdName = name
    , cdRevealedName = Nothing
    , cdCost = Nothing
    , cdAdditionalCost = Nothing
    , cdLevel = 0
    , cdCardType = ScenarioType
    , cdCardSubType = Nothing
    , cdClassSymbols = mempty
    , cdSkills = mempty
    , cdCardTraits = mempty
    , cdRevealedCardTraits = mempty
    , cdKeywords = mempty
    , cdFastWindow = Nothing
    , cdActions = mempty
    , cdRevelation = False
    , cdVictoryPoints = Nothing
    , cdVengeancePoints = Nothing
    , cdCriteria = Nothing
    , cdOverrideActionPlayableIfCriteriaMet = False
    , cdCommitRestrictions = mempty
    , cdAttackOfOpportunityModifiers = mempty
    , cdPermanent = False
    , cdEncounterSet = Just ecSet
    , cdEncounterSetQuantity = Nothing
    , cdUnique = True
    , cdDoubleSided = True
    , cdLimits = []
    , cdExceptional = False
    , cdUses = NoUses
    , cdPlayableFromDiscard = False
    , cdStage = Nothing
    , cdSlots = mempty
    , cdCardInHandEffects = False
    , cdCardInDiscardEffects = False
    , cdCardInSearchEffects = False
    , cdAlternateCardCodes = mempty
    , cdArt = unCardCode cCode
    , cdLocationSymbol = Nothing
    , cdLocationRevealedSymbol = Nothing
    , cdLocationConnections = mempty
    , cdLocationRevealedConnections = mempty
    , cdPurchaseMentalTrauma = Nothing
    , cdCanReplace = True
    , cdDeckRestrictions = []
    }

allScenarioCards :: Map CardCode CardDef
allScenarioCards =
  mapFromList $ flip map (mapToList allScenarios) $ \(c, SomeScenario s) -> do
    let ecSet = fromJustNote "you forgot to add the encounter set" $ lookup c scenarioEncounterSets
        name = scenarioName $ toAttrs $ Scenario (s Easy)
    (c, scenarioCard c name ecSet)

allScenarios :: Map CardCode SomeScenario
allScenarios =
  mapFromList
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
    , ("04161", SomeScenario theBoundaryBeyond)
    , ("04205", SomeScenario heartOfTheElders)
    , ("04237", SomeScenario theCityOfArchives)
    , ("04277", SomeScenario theDepthsOfYoth)
    , ("04314", SomeScenario shatteredAeons)
    , ("04344", SomeScenario turnBackTime)
    , ("05043", SomeScenario disappearanceAtTheTwilightEstate)
    , ("05050", SomeScenario theWitchingHour)
    , ("05065", SomeScenario atDeathsDoorstep)
    , ("05120", SomeScenario theSecretName)
    , ("05161", SomeScenario theWagesOfSin)
    , ("05197", SomeScenario forTheGreaterGood)
    , ("05238", SomeScenario unionAndDisillusion)
    , ("50011", SomeScenario returnToTheGathering)
    , ("50025", SomeScenario returnToTheMidnightMasks)
    , ("50032", SomeScenario returnToTheDevourerBelow)
    , ("81001", SomeScenario curseOfTheRougarou)
    , ("82001", SomeScenario carnevaleOfHorrors)
    ]

scenarioEncounterSets :: Map CardCode EncounterSet
scenarioEncounterSets =
  mapFromList
    [ ("01104", EncounterSet.TheGathering)
    , ("01120", EncounterSet.TheMidnightMasks)
    , ("01142", EncounterSet.TheDevourerBelow)
    , ("02041", EncounterSet.ExtracurricularActivity)
    , ("02062", EncounterSet.TheHouseAlwaysWins)
    , ("02118", EncounterSet.TheMiskatonicMuseum)
    , ("02159", EncounterSet.TheEssexCountyExpress)
    , ("02195", EncounterSet.BloodOnTheAltar)
    , ("02236", EncounterSet.UndimensionedAndUnseen)
    , ("02274", EncounterSet.WhereDoomAwaits)
    , ("02311", EncounterSet.LostInTimeAndSpace)
    , ("03043", EncounterSet.CurtainCall)
    , ("03061", EncounterSet.TheLastKing)
    , ("03120", EncounterSet.EchoesOfThePast)
    , ("03159", EncounterSet.TheUnspeakableOath)
    , ("03200", EncounterSet.APhantomOfTruth)
    , ("03240", EncounterSet.ThePallidMask)
    , ("03274", EncounterSet.BlackStarsRise)
    , ("03316", EncounterSet.DimCarcosa)
    , ("04043", EncounterSet.TheUntamedWilds)
    , ("04054", EncounterSet.TheDoomOfEztli)
    , ("04113", EncounterSet.ThreadsOfFate)
    , ("04161", EncounterSet.TheBoundaryBeyond)
    , ("04205", EncounterSet.HeartOfTheElders)
    , ("04237", EncounterSet.TheCityOfArchives)
    , ("04277", EncounterSet.TheDepthsOfYoth)
    , ("04314", EncounterSet.ShatteredAeons)
    , ("04344", EncounterSet.TurnBackTime)
    , ("05043", EncounterSet.DisappearanceAtTheTwilightEstate)
    , ("05050", EncounterSet.TheWitchingHour)
    , ("05065", EncounterSet.AtDeathsDoorstep)
    , ("05120", EncounterSet.TheSecretName)
    , ("05161", EncounterSet.TheWagesOfSin)
    , ("05197", EncounterSet.ForTheGreaterGood)
    , ("05238", EncounterSet.UnionAndDisillusion)
    , ("50011", EncounterSet.ReturnToTheGathering)
    , ("50025", EncounterSet.ReturnToTheMidnightMasks)
    , ("50032", EncounterSet.ReturnToTheDevourerBelow)
    , ("81001", EncounterSet.CurseOfTheRougarou)
    , ("82001", EncounterSet.CarnevaleOfHorrors)
    ]
