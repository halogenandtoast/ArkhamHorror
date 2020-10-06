module Arkham.Types.Card.PlayerCard.Attrs
  ( Attrs(..)
  , BearerId(..)
  , AttackOfOpportunityModifier(..)
  , playerCardMatch
  , basePlayerCard
  , asset
  , event
  , skill
  , treachery
  , enemy
  )
where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Action
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Type
import Arkham.Types.ClassSymbol
import Arkham.Types.CommitRestriction
import Arkham.Types.Keyword
import Arkham.Types.SkillType
import Arkham.Types.Trait
import Arkham.Types.Window

data AttackOfOpportunityModifier = DoesNotProvokeAttacksOfOpportunity
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Attrs = Attrs
  { pcCardCode :: CardCode
  , pcName :: Text
  , pcCost :: CardCost
  , pcLevel :: Int
  , pcCardType :: PlayerCardType
  , pcWeakness :: Bool
  , pcBearer :: Maybe BearerId
  , pcClassSymbol :: ClassSymbol
  , pcSkills :: [SkillType]
  , pcTraits :: [Trait]
  , pcKeywords :: [Keyword]
  , pcFast :: Bool
  , pcWindows :: HashSet Window
  , pcId :: CardId
  , pcAction :: Maybe Action
  , pcRevelation :: Bool
  , pcVictoryPoints :: Maybe Int
  , pcCommitRestrictions :: [CommitRestriction]
  , pcAttackOfOpportunityModifiers :: [AttackOfOpportunityModifier]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "pc"
  toEncoding = genericToEncoding $ aesonOptions $ Just "pc"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "pc"

playerCardMatch :: (PlayerCardType, Maybe Trait) -> Attrs -> Bool
playerCardMatch (cardType, mtrait) Attrs {..} =
  pcCardType == cardType && maybe True (`elem` pcTraits) mtrait

basePlayerCard
  :: CardId -> CardCode -> Text -> Int -> PlayerCardType -> ClassSymbol -> Attrs
basePlayerCard cardId cardCode name cost cardType classSymbol = Attrs
  { pcCardCode = cardCode
  , pcName = name
  , pcCost = StaticCost cost
  , pcLevel = 0
  , pcCardType = cardType
  , pcWeakness = False
  , pcBearer = Nothing
  , pcClassSymbol = classSymbol
  , pcSkills = mempty
  , pcTraits = mempty
  , pcKeywords = mempty
  , pcFast = False
  , pcWindows = mempty
  , pcId = cardId
  , pcAction = Nothing
  , pcRevelation = False
  , pcVictoryPoints = Nothing
  , pcCommitRestrictions = mempty
  , pcAttackOfOpportunityModifiers = mempty
  }

asset :: CardId -> CardCode -> Text -> Int -> ClassSymbol -> Attrs
asset cardId cardCode name cost classSymbol =
  basePlayerCard cardId cardCode name cost AssetType classSymbol

event :: CardId -> CardCode -> Text -> Int -> ClassSymbol -> Attrs
event cardId cardCode name cost classSymbol =
  basePlayerCard cardId cardCode name cost EventType classSymbol

skill :: CardId -> CardCode -> Text -> [SkillType] -> ClassSymbol -> Attrs
skill cardId cardCode name skills classSymbol =
  (basePlayerCard cardId cardCode name 0 SkillType classSymbol)
    { pcSkills = skills
    }

treachery :: CardId -> CardCode -> Text -> Int -> Attrs
treachery cardId cardCode name cost =
  (basePlayerCard cardId cardCode name cost PlayerTreacheryType Neutral)
    { pcWeakness = True
    }

enemy :: CardId -> CardCode -> Text -> Int -> Attrs
enemy cardId cardCode name cost =
  (basePlayerCard cardId cardCode name cost PlayerEnemyType Neutral)
    { pcWeakness = True
    }

