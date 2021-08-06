module Arkham.PlayerCard
  ( lookupPlayerCardDef
  , lookupPlayerCardName
  , allPlayerCards
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards (allPlayerAssetCards)
import Arkham.Enemy.Cards (allPlayerEnemyCards)
import Arkham.Event.Cards (allPlayerEventCards)
import Arkham.Skill.Cards (allPlayerSkillCards)
import Arkham.Treachery.Cards (allPlayerTreacheryCards)
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardDef
import Arkham.Types.Name

lookupPlayerCardName :: CardCode -> Name
lookupPlayerCardName = cdName . lookupPlayerCardDef

lookupPlayerCardDef :: CardCode -> CardDef
lookupPlayerCardDef cardCode =
  fromJustNote ("Unknown card: " <> show cardCode)
    $ lookup cardCode allPlayerCards

allPlayerCards :: Map CardCode CardDef
allPlayerCards =
  allPlayerEnemyCards
    <> allPlayerTreacheryCards
    <> allPlayerAssetCards
    <> allPlayerEventCards
    <> allPlayerSkillCards
