module Arkham.PlayerCard
  ( lookupPlayerCardDef
  , lookupPlayerCardName
  , allPlayerCards
  , allWeaknesses
  , randomWeakness
  ) where

import Arkham.Prelude

import Arkham.Types.Card.CardType
import Arkham.Asset.Cards (allPlayerAssetCards)
import Arkham.Enemy.Cards (allPlayerEnemyCards)
import Arkham.Event.Cards (allPlayerEventCards)
import Arkham.Skill.Cards (allPlayerSkillCards)
import Arkham.Treachery.Cards (allPlayerTreacheryCards)
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardDef
import Arkham.Types.Name
import Arkham.Types.Asset.Uses

lookupPlayerCardName :: CardCode -> Name
lookupPlayerCardName = cdName . lookupPlayerCardDef

lookupPlayerCardDef :: CardCode -> CardDef
lookupPlayerCardDef cardCode =
  fromJustNote ("Unknown card: " <> show cardCode)
    $ lookup cardCode allPlayerCards

allWeaknesses :: [CardDef]
allWeaknesses = filter (and . sequence [cdWeakness, (/= "01000") . cdCardCode]) . toList $ allPlayerCards

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
  , cdName = "Random Weakness"
  , cdRevealedName = Nothing
  , cdCost = Nothing
  , cdLevel = 0
  , cdCardType = PlayerTreacheryType
  , cdWeakness = True
  , cdClassSymbol = Nothing
  , cdSkills = mempty
  , cdCardTraits = mempty
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

