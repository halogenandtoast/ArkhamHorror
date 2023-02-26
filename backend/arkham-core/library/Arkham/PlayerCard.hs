module Arkham.PlayerCard
  ( lookupPlayerCardDef
  , lookupPlayerCardName
  , allPlayerCards
  , allBasicWeaknesses
  , randomWeakness
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards (allPlayerAssetCards, allSpecialPlayerAssetCards)
import Arkham.Enemy.Cards (allPlayerEnemyCards)
import Arkham.Event.Cards (allPlayerEventCards)
import Arkham.Skill.Cards (allPlayerSkillCards)
import Arkham.Treachery.Cards (allPlayerTreacheryCards)
import Arkham.Asset.Uses
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.ClassSymbol
import Arkham.Name
import Data.HashMap.Strict qualified as HashMap

lookupPlayerCardName :: CardCode -> Name
lookupPlayerCardName = withCardDef cdName . lookupPlayerCardDef

lookupPlayerCardDef :: CardCode -> SomeCardDef
lookupPlayerCardDef cardCode =
  fromJustNote ("Unknown card: " <> show cardCode)
    $ lookup cardCode allPlayerCards

allBasicWeaknesses :: [SomeCardDef]
allBasicWeaknesses =
  filter ((== Just BasicWeakness) . withCardDef cdCardSubType) . toList $ allPlayerCards

allPlayerCards :: HashMap CardCode SomeCardDef
allPlayerCards =
  HashMap.map (SomeCardDef SPlayerEnemyType) allPlayerEnemyCards
    <> HashMap.map (SomeCardDef SPlayerTreacheryType) allPlayerTreacheryCards
    <> HashMap.map (SomeCardDef SAssetType) allPlayerAssetCards
    <> HashMap.map (SomeCardDef SAssetType) allSpecialPlayerAssetCards
    <> HashMap.map (SomeCardDef SEventType) allPlayerEventCards
    <> HashMap.map (SomeCardDef SSkillType) allPlayerSkillCards
    <> singletonMap "01000" (SomeCardDef SPlayerTreacheryType randomWeakness)

randomWeakness :: CardDef 'PlayerTreacheryType
randomWeakness = CardDef
  { cdCardCode = "01000"
  , cdName = "Random Basic Weakness"
  , cdRevealedName = Nothing
  , cdCost = Nothing
  , cdAdditionalCost = Nothing
  , cdLevel = 0
  , cdCardSubType = Just Weakness
  , cdClassSymbols = singleton Neutral
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
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdUnique = False
  , cdDoubleSided = False
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
  , cdArt = "01000"
  , cdLocationSymbol = Nothing
  , cdLocationRevealedSymbol = Nothing
  , cdLocationConnections = []
  , cdLocationRevealedConnections = []
  , cdPurchaseMentalTrauma = Nothing
  , cdCanReplace = True
  }

