{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Types.Card.CardDef where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Action (Action)
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardMatcher
import Arkham.Types.Card.CardType
import Arkham.Types.Card.Cost
import Arkham.Types.ClassSymbol
import Arkham.Types.CommitRestriction
import Arkham.Types.EncounterSet
import Arkham.Types.Id
import Arkham.Types.Keyword (HasKeywords(..), Keyword)
import Arkham.Types.Matcher
import Arkham.Types.Name
import Arkham.Types.SkillType
import Arkham.Types.Trait
import Arkham.Types.WindowMatcher (WindowMatcher)

data AttackOfOpportunityModifier = DoesNotProvokeAttacksOfOpportunity
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EventChoicesRepeatable = EventChoicesRepeatable | EventChoicesNotRepeatable
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EventChoice = EventChooseN Int EventChoicesRepeatable
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CardDef = CardDef
  { cdCardCode :: CardCode
  , cdName :: Name
  , cdRevealedName :: Maybe Name
  , cdCost :: Maybe CardCost
  , cdLevel :: Int
  , cdCardType :: CardType
  , cdWeakness :: Bool
  , cdClassSymbol :: Maybe ClassSymbol
  , cdSkills :: [SkillType]
  , cdCardTraits :: Set Trait
  , cdKeywords :: Set Keyword
  , cdFastWindow :: Maybe WindowMatcher
  , cdAction :: Maybe Action
  , cdRevelation :: Bool
  , cdVictoryPoints :: Maybe Int
  , cdPlayRestrictions :: Maybe PlayRestriction
  , cdCommitRestrictions :: [CommitRestriction]
  , cdAttackOfOpportunityModifiers :: [AttackOfOpportunityModifier]
  , cdPermanent :: Bool
  , cdEncounterSet :: Maybe EncounterSet
  , cdEncounterSetQuantity :: Maybe Int
  , cdUnique :: Bool
  , cdDoubleSided :: Bool
  , cdLimits :: [CardLimit]
  , cdExceptional :: Bool
  }
  deriving stock (Show, Eq, Generic)

data CardLimit = LimitPerInvestigator Int | LimitPerTrait Trait Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Named CardDef where
  toName = cdName

weaknessL :: Lens' CardDef Bool
weaknessL = lens cdWeakness $ \m x -> m { cdWeakness = x }

keywordsL :: Lens' CardDef (Set Keyword)
keywordsL = lens cdKeywords $ \m x -> m { cdKeywords = x }

cardTraitsL :: Lens' CardDef (Set Trait)
cardTraitsL = lens cdCardTraits $ \m x -> m { cdCardTraits = x }

instance ToJSON CardDef where
  toJSON = genericToJSON $ aesonOptions $ Just "cd"
  toEncoding = genericToEncoding $ aesonOptions $ Just "cd"

instance FromJSON CardDef where
  parseJSON = genericParseJSON $ aesonOptions $ Just "cd"

class GetCardDef env a where
  getCardDef :: MonadReader env m => a -> m CardDef

class HasCardDef a where
  toCardDef :: a -> CardDef

class HasOriginalCardCode a where
  toOriginalCardCode :: a -> CardCode

class HasCardType a where
  toCardType :: a -> CardType

instance HasCardDef a => HasCardType a where
  toCardType = cdCardType . toCardDef

instance {-# OVERLAPPABLE #-} HasCardDef a => HasTraits a where
  toTraits = cdCardTraits . toCardDef

instance HasCardDef a => HasKeywords a where
  toKeywords = cdKeywords . toCardDef

instance HasCardDef CardDef where
  toCardDef = id

instance HasCardCode CardDef where
  toCardCode = cdCardCode

cardMatch :: (HasCardCode a, HasCardDef a) => a -> CardMatcher -> Bool
cardMatch a = \case
  AnyCard -> True
  CardWithType cardType' -> toCardType a == cardType'
  CardWithCardCode cardCode -> toCardCode a == cardCode
  CardWithTitle title -> (nameTitle . cdName $ toCardDef a) == title
  CardWithTrait trait -> trait `member` toTraits a
  CardWithClass role -> cdClassSymbol (toCardDef a) == Just role
  CardMatches ms -> all (cardMatch a) ms
  CardWithOneOf ms -> any (cardMatch a) ms
  CardWithoutKeyword k -> k `notMember` cdKeywords (toCardDef a)
  NonWeakness -> not . cdWeakness $ toCardDef a
  NonExceptional -> not . cdExceptional $ toCardDef a

testCardDef :: CardType -> CardCode -> CardDef
testCardDef cardType cardCode = CardDef
  { cdCardCode = cardCode
  , cdName = "Test"
  , cdRevealedName = Nothing
  , cdCost = Nothing
  , cdLevel = 0
  , cdCardType = cardType
  , cdWeakness = False
  , cdClassSymbol = Nothing
  , cdSkills = []
  , cdCardTraits = mempty
  , cdKeywords = mempty
  , cdFastWindow = Nothing
  , cdAction = Nothing
  , cdRevelation = False
  , cdVictoryPoints = Nothing
  , cdPlayRestrictions = Nothing
  , cdCommitRestrictions = []
  , cdAttackOfOpportunityModifiers = []
  , cdPermanent = False
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdUnique = False
  , cdDoubleSided = False
  , cdLimits = []
  , cdExceptional = False
  }

data DiscardSignifier = AnyPlayerDiscard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data PlayRestriction
  = AnotherInvestigatorInSameLocation
  | InvestigatorIsAlone
  | ScenarioCardHasResignAbility
  | ClueOnLocation
  | FirstAction
  | OnLocation LocationId
  | CardExists CardMatcher
  | ExtendedCardExists ExtendedCardMatcher
  | PlayableCardExists ExtendedCardMatcher
  | AssetExists AssetMatcher
  | InvestigatorExists InvestigatorMatcher
  | EnemyExists EnemyMatcher
  | NoEnemyExists EnemyMatcher
  | LocationExists LocationMatcher
  | OwnCardWithDoom
  | CardInDiscard DiscardSignifier [Trait]
  | ReturnableCardInDiscard DiscardSignifier [Trait]
  | PlayRestrictions [PlayRestriction]
  | AnyPlayRestriction [PlayRestriction]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup PlayRestriction where
  PlayRestrictions xs <> PlayRestrictions ys = PlayRestrictions $ xs <> ys
  PlayRestrictions xs <> x = PlayRestrictions $ x : xs
  x <> PlayRestrictions xs = PlayRestrictions $ x : xs
  x <> y = PlayRestrictions [x, y]
