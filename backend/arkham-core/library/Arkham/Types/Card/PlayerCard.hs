module Arkham.Types.Card.PlayerCard where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Action (Action)
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.Class
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.InvestigatorId
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

newtype BearerId = BearerId { unBearerId :: InvestigatorId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, IsString)

newtype DiscardedPlayerCard = DiscardedPlayerCard { unDiscardedPlayerCard :: PlayerCard }

data AttackOfOpportunityModifier = DoesNotProvokeAttacksOfOpportunity
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data PlayerCard = MkPlayerCard
  { pcId :: CardId
  , pcBearer :: Maybe InvestigatorId
  , pcDef :: PlayerCardDef
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Hashable

data PlayerCardDef = PlayerCardDef
  { pcCardCode :: CardCode
  , pcName :: Text
  , pcCost :: CardCost
  , pcLevel :: Int
  , pcCardType :: PlayerCardType
  , pcWeakness :: Bool
  , pcClassSymbol :: ClassSymbol
  , pcSkills :: [SkillType]
  , pcTraits :: HashSet Trait
  , pcKeywords :: HashSet Keyword
  , pcFast :: Bool
  , pcWindows :: HashSet Window
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

instance ToJSON PlayerCardDef where
  toJSON = genericToJSON $ aesonOptions $ Just "pc"
  toEncoding = genericToEncoding $ aesonOptions $ Just "pc"

instance FromJSON PlayerCard where
  parseJSON = genericParseJSON $ aesonOptions $ Just "pc"

instance FromJSON PlayerCardDef where
  parseJSON = genericParseJSON $ aesonOptions $ Just "pc"

instance HasSkillIcons PlayerCard where
  getSkillIcons = pcSkills . pcDef

instance HasCost PlayerCard where
  getCost c = case pcCost (pcDef c) of
    StaticCost n -> n
    DynamicCost -> 0

class HasCardTraits a where
  traitsL :: Lens' a (HashSet Trait)

instance HasCardTraits PlayerCardDef where
  traitsL = lens pcTraits $ \m x -> m { pcTraits = x }

defL :: Lens' PlayerCard PlayerCardDef
defL = lens pcDef $ \m x -> m { pcDef = x }


instance HasCardTraits PlayerCard where
  traitsL = defL . traitsL

playerCardMatch :: (PlayerCardType, HashSet Trait) -> PlayerCardDef -> Bool
playerCardMatch (cardType, traits) PlayerCardDef {..} =
  pcCardType
    == cardType
    && (null traits || notNull (intersection pcTraits traits))
