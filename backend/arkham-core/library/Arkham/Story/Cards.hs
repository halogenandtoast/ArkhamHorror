module Arkham.Story.Cards where

import Arkham.Prelude

import Arkham.Types.Asset.Uses
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardDef
import Arkham.Types.Card.CardType
import Arkham.Types.EncounterSet
import Arkham.Types.Name

story :: CardCode -> Name -> EncounterSet -> CardDef
story cardCode name encounterSet = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdRevealedName = Nothing
  , cdCost = Nothing
  , cdLevel = 0
  , cdCardType = StoryType
  , cdCardSubType = Nothing
  , cdClassSymbol = Nothing
  , cdSkills = mempty
  , cdCardTraits = mempty
  , cdRevealedCardTraits = mempty
  , cdKeywords = mempty
  , cdFastWindow = Nothing
  , cdAction = Nothing
  , cdRevelation = False
  , cdVictoryPoints = Nothing
  , cdCriteria = mempty
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
