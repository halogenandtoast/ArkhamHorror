module Arkham.PlayerCard
  ( lookupPlayerCardDef
  , lookupPlayerCardName
  , allPlayerCards
  , allBasicWeaknesses
  , randomWeakness
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards (allPlayerAssetCards)
import Arkham.Enemy.Cards (allPlayerEnemyCards)
import Arkham.Event.Cards (allPlayerEventCards)
import Arkham.Skill.Cards (allPlayerSkillCards)
import Arkham.Treachery.Cards (allPlayerTreacheryCards)
import Arkham.Types.Asset.Uses
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardDef
import Arkham.Types.Card.CardType
import Arkham.Types.ClassSymbol
import Arkham.Types.Name

lookupPlayerCardName :: CardCode -> Name
lookupPlayerCardName = cdName . lookupPlayerCardDef

lookupPlayerCardDef :: CardCode -> CardDef
lookupPlayerCardDef cardCode =
  fromJustNote ("Unknown card: " <> show cardCode)
    $ lookup cardCode allPlayerCards

allBasicWeaknesses :: [CardDef]
allBasicWeaknesses =
  filter ((== Just BasicWeakness) . cdCardSubType) . toList $ allPlayerCards

allPlayerCards :: HashMap CardCode CardDef
allPlayerCards =
  allPlayerEnemyCards
    <> allPlayerTreacheryCards
    <> allPlayerAssetCards
    <> allPlayerEventCards
    <> allPlayerSkillCards
    <> singletonMap "01000" randomWeakness

randomWeakness :: CardDef
randomWeakness = CardDef
  { cdCardCode = "01000"
  , cdName = "Random Basic Weakness"
  , cdRevealedName = Nothing
  , cdCost = Nothing
  , cdLevel = 0
  , cdCardType = PlayerTreacheryType
  , cdCardSubType = Just Weakness
  , cdClassSymbol = Just Neutral
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
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdUnique = False
  , cdDoubleSided = False
  , cdLimits = []
  , cdExceptional = False
  , cdUses = NoUses
  }

