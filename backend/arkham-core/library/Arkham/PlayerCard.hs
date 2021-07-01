module Arkham.PlayerCard
  ( lookupPlayerCard
  , lookupPlayerCardDef
  , genPlayerCard
  , lookupPlayerCardName
  , allPlayerCards
  , basePlayerCard
  )
where

import Arkham.Prelude

import Arkham.Asset.Cards (allPlayerAssetCards)
import Arkham.Enemy.Cards (allPlayerEnemyCards)
import Arkham.Event.Cards (allEventCards)
import Arkham.Skill.Cards (allPlayerSkillCards)
import Arkham.Treachery.Cards (allPlayerTreacheryCards)
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardDef
import Arkham.Types.Card.CardType
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard
import Arkham.Types.ClassSymbol
import Arkham.Types.Name

genPlayerCard :: MonadRandom m => CardCode -> m PlayerCard
genPlayerCard cardCode = lookupPlayerCard cardCode <$> getRandom

lookupPlayerCardName :: CardCode -> Name
lookupPlayerCardName = cdName . lookupPlayerCardDef

lookupPlayerCard :: CardCode -> CardId -> PlayerCard
lookupPlayerCard cardCode cardId = MkPlayerCard
  { pcId = cardId
  , pcDef = lookupPlayerCardDef cardCode
  , pcBearer = Nothing
  }

lookupPlayerCardDef :: CardCode -> CardDef
lookupPlayerCardDef cardCode =
  fromJustNote ("Unknown card: " <> show cardCode)
    $ lookup cardCode allPlayerCards

allPlayerCards :: HashMap CardCode CardDef
allPlayerCards = allPlayerEnemyCards <> allPlayerTreacheryCards <> allPlayerAssetCards <> allEventCards <> allPlayerSkillCards

basePlayerCard
  :: CardCode
  -> Name
  -> Int
  -> CardType
  -> ClassSymbol
  -> CardDef
basePlayerCard cardCode name cost cardType classSymbol = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdCost = Just (StaticCost cost)
  , cdLevel = 0
  , cdCardType = cardType
  , cdWeakness = False
  , cdClassSymbol = Just classSymbol
  , cdSkills = mempty
  , cdCardTraits = mempty
  , cdKeywords = mempty
  , cdFast = False
  , cdWindows = mempty
  , cdAction = Nothing
  , cdRevelation = False
  , cdVictoryPoints = Nothing
  , cdCommitRestrictions = mempty
  , cdAttackOfOpportunityModifiers = mempty
  , cdPermanent = False
  , cdEncounterSet = Nothing
  , cdUnique = False
  }
