module Arkham.Types.Card.PlayerCard where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Action (Action)
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.Class
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.ClassSymbol
import Arkham.Types.CommitRestriction
import Arkham.Types.Keyword (Keyword)
import Arkham.Types.SkillType
import Arkham.Types.Trait
import Arkham.Types.Window

data PlayerCardType
  = AssetType
  | EventType
  | SkillType
  | PlayerTreacheryType
  | PlayerEnemyType
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

newtype BearerId = BearerId { unBearerId :: CardCode }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, IsString)

newtype DiscardedPlayerCard = DiscardedPlayerCard { unDiscardedPlayerCard :: PlayerCard }

data AttackOfOpportunityModifier = DoesNotProvokeAttacksOfOpportunity
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data PlayerCard = MkPlayerCard
  { pcCardCode :: CardCode
  , pcName :: Text
  , pcCost :: CardCost
  , pcLevel :: Int
  , pcCardType :: PlayerCardType
  , pcWeakness :: Bool
  , pcBearer :: Maybe BearerId
  , pcClassSymbol :: ClassSymbol
  , pcSkills :: [SkillType]
  , pcTraits :: HashSet Trait
  , pcKeywords :: HashSet Keyword
  , pcFast :: Bool
  , pcWindows :: HashSet Window
  , pcId :: CardId
  , pcAction :: Maybe Action
  , pcRevelation :: Bool
  , pcVictoryPoints :: Maybe Int
  , pcCommitRestrictions :: [CommitRestriction]
  , pcAttackOfOpportunityModifiers :: [AttackOfOpportunityModifier]
  , pcPermanent :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Hashable

instance ToJSON PlayerCard where
  toJSON = genericToJSON $ aesonOptions $ Just "pc"
  toEncoding = genericToEncoding $ aesonOptions $ Just "pc"

instance FromJSON PlayerCard where
  parseJSON = genericParseJSON $ aesonOptions $ Just "pc"

instance HasSkillIcons PlayerCard where
  getSkillIcons = pcSkills

instance HasCost PlayerCard where
  getCost c = case pcCost c of
    StaticCost n -> n
    DynamicCost -> 0

traits :: Lens' PlayerCard (HashSet Trait)
traits = lens pcTraits $ \m x -> m { pcTraits = x }

playerCardMatch :: (PlayerCardType, Maybe Trait) -> PlayerCard -> Bool
playerCardMatch (cardType, mtrait) MkPlayerCard {..} =
  pcCardType == cardType && maybe True (`elem` pcTraits) mtrait
