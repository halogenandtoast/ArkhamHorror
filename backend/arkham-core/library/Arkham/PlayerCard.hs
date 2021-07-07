module Arkham.PlayerCard
  ( lookupPlayerCard
  , lookupPlayerCardDef
  , genPlayerCard
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
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard
import Arkham.Types.Name

genPlayerCard :: MonadRandom m => CardDef -> m PlayerCard
genPlayerCard cardDef = lookupPlayerCard cardDef <$> getRandom

lookupPlayerCardName :: CardCode -> Name
lookupPlayerCardName = cdName . lookupPlayerCardDef

lookupPlayerCard :: CardDef -> CardId -> PlayerCard
lookupPlayerCard cardDef cardId =
  MkPlayerCard { pcId = cardId, pcDef = cardDef, pcBearer = Nothing }

lookupPlayerCardDef :: CardCode -> CardDef
lookupPlayerCardDef cardCode =
  fromJustNote ("Unknown card: " <> show cardCode)
    $ lookup cardCode allPlayerCards

allPlayerCards :: HashMap CardCode CardDef
allPlayerCards =
  allPlayerEnemyCards
    <> allPlayerTreacheryCards
    <> allPlayerAssetCards
    <> allPlayerEventCards
    <> allPlayerSkillCards
