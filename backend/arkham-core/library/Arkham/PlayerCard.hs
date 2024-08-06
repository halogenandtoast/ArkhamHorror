module Arkham.PlayerCard (
  lookupPlayerCardDef,
  lookupPlayerCardName,
  allPlayerCards,
  allBasicWeaknesses,
  randomWeakness,
) where

import Arkham.Prelude

import Arkham.Asset.Cards (allPlayerAssetCards, allSpecialPlayerAssetCards)
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.ClassSymbol
import Arkham.Enemy.Cards (allPlayerEnemyCards)
import Arkham.Event.Cards (allPlayerEventCards)
import Arkham.Name
import Arkham.Skill.Cards (allPlayerSkillCards)
import Arkham.Story.Cards (realityAcid)
import Arkham.Treachery.Cards (allPlayerTreacheryCards)

lookupPlayerCardName :: CardCode -> Name
lookupPlayerCardName = cdName . lookupPlayerCardDef

lookupPlayerCardDef :: CardCode -> CardDef
lookupPlayerCardDef cardCode =
  fromJustNote ("Unknown card: " <> show cardCode)
    $ lookup cardCode allPlayerCards

allBasicWeaknesses :: [CardDef]
allBasicWeaknesses =
  filter ((== Just BasicWeakness) . cdCardSubType) . toList $ allPlayerCards

allPlayerCards :: Map CardCode CardDef
allPlayerCards =
  allPlayerEnemyCards
    <> allPlayerTreacheryCards
    <> allPlayerAssetCards
    <> allSpecialPlayerAssetCards
    <> allPlayerEventCards
    <> allPlayerSkillCards
    <> singletonMap "89005" realityAcid
    <> singletonMap "01000" randomWeakness

randomWeakness :: CardDef
randomWeakness =
  (emptyCardDef "01000" "Random Basic Weakness" PlayerTreacheryType)
    { cdCardSubType = Just Weakness
    , cdClassSymbols = singleton Neutral
    , cdLevel = Nothing
    }
