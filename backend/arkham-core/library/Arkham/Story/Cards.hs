module Arkham.Story.Cards where

import Arkham.Prelude

import Arkham.Asset.Uses
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.EncounterSet
import Arkham.Name

story :: CardCode -> Name -> EncounterSet -> CardDef
story cardCode name encounterSet = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdRevealedName = Nothing
  , cdCost = Nothing
  , cdAdditionalCost = Nothing
  , cdLevel = 0
  , cdCardType = StoryType
  , cdCardSubType = Nothing
  , cdClassSymbols = mempty
  , cdSkills = mempty
  , cdCardTraits = mempty
  , cdRevealedCardTraits = mempty
  , cdKeywords = mempty
  , cdFastWindow = Nothing
  , cdActions = []
  , cdRevelation = False
  , cdVictoryPoints = Nothing
  , cdVengeancePoints = Nothing
  , cdCriteria = mempty
  , cdOverrideActionPlayableIfCriteriaMet = False
  , cdCommitRestrictions = mempty
  , cdAttackOfOpportunityModifiers = mempty
  , cdPermanent = False
  , cdEncounterSet = Just encounterSet
  , cdEncounterSetQuantity = Just 1
  , cdUnique = False
  , cdDoubleSided = True
  , cdLimits = []
  , cdExceptional = False
  , cdUses = NoUses
  , cdPlayableFromDiscard = False
  , cdStage = Nothing
  , cdSlots = []
  , cdCardInHandEffects = False
  , cdCardInDiscardEffects = False
  , cdCardInSearchEffects = False
  , cdAlternateCardCodes = []
  , cdArt = unCardCode cardCode
  , cdLocationSymbol = Nothing
  , cdLocationRevealedSymbol = Nothing
  , cdLocationConnections = []
  , cdLocationRevealedConnections = []
  , cdPurchaseMentalTrauma = Nothing
  }

allStoryCards :: HashMap CardCode CardDef
allStoryCards = mapFromList $ map
  (toCardCode &&& id)
  [ sickeningReality_65
  , sickeningReality_66
  , sickeningReality_67
  , sickeningReality_68
  , sickeningReality_69
  , engramsOath
  , langneauPerdu
  , thePattern
  , theFirstShow
  , aboveAndBelow
  , songsThatTheHyadesShallSing
  , starsOfAldebaran
  , bleakDesolation
  , inhabitantOfCarcosa
  , aMomentsRest
  , theCoffin
  , mappingTheStreets
  , theKingsParade
  , theArchway
  , theHeightOfTheDepths
  , stepsOfThePalace
  , theFall
  , hastursEnd
  ]

sickeningReality_65 :: CardDef
sickeningReality_65 = story "03065" "Sickening Reality" TheLastKing

sickeningReality_66 :: CardDef
sickeningReality_66 = story "03066" "Sickening Reality" TheLastKing

sickeningReality_67 :: CardDef
sickeningReality_67 = story "03067" "Sickening Reality" TheLastKing

sickeningReality_68 :: CardDef
sickeningReality_68 = story "03068" "Sickening Reality" TheLastKing

sickeningReality_69 :: CardDef
sickeningReality_69 = story "03069" "Sickening Reality" TheLastKing

engramsOath :: CardDef
engramsOath = story "03076b" "Engram's Oath" TheLastKing

langneauPerdu :: CardDef
langneauPerdu = story "03077b" "L'angneau Perdu" TheLastKing

thePattern :: CardDef
thePattern = story "03078b" "The Pattern" TheLastKing

theFirstShow :: CardDef
theFirstShow = story "03079b" "The First Show" TheLastKing

aboveAndBelow :: CardDef
aboveAndBelow = story "03080b" "Above and Below" TheLastKing

songsThatTheHyadesShallSing :: CardDef
songsThatTheHyadesShallSing =
  story "03325c" "Songs That the Hyades Shall Sing" DimCarcosa

starsOfAldebaran :: CardDef
starsOfAldebaran = story "03326c" "Stars of Aldebaran" DimCarcosa

bleakDesolation :: CardDef
bleakDesolation = story "03326e" "Bleak Desolation" DimCarcosa

inhabitantOfCarcosa :: CardDef
inhabitantOfCarcosa = story "03327c" "Inhabitant of Carcosa" DimCarcosa

aMomentsRest :: CardDef
aMomentsRest = story "03327e" "A Moment's Rest" DimCarcosa

theCoffin :: CardDef
theCoffin = story "03327g" "The Coffin" DimCarcosa

mappingTheStreets :: CardDef
mappingTheStreets = story "03328c" "Mapping the Streets" DimCarcosa

theKingsParade :: CardDef
theKingsParade = story "03328e" "The King's Parade" DimCarcosa

theArchway :: CardDef
theArchway = story "03328g" "The Archway" DimCarcosa

theHeightOfTheDepths :: CardDef
theHeightOfTheDepths = story "03329c" "The Height of the Deapths" DimCarcosa

stepsOfThePalace :: CardDef
stepsOfThePalace = story "03329e" "Steps of the Palace" DimCarcosa

theFall :: CardDef
theFall = story "03330c" "The Fall" DimCarcosa

hastursEnd :: CardDef
hastursEnd = story "03331c" "Hastur's End" DimCarcosa
