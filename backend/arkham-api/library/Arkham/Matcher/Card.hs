{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.Card where

import Arkham.Action
import Arkham.Card.CardCode
import Arkham.Card.CardType
import Arkham.Card.Id
import Arkham.ClassSymbol
import Arkham.Cost.Status
import Arkham.Criteria.Override
import Arkham.Deck
import Arkham.EncounterSet (EncounterSet)
import Arkham.Id
import Arkham.Keyword (Keyword)
import Arkham.LocationSymbol
import {-# SOURCE #-} Arkham.Matcher.Ability
import {-# SOURCE #-} Arkham.Matcher.Asset
import Arkham.Matcher.Base
import {-# SOURCE #-} Arkham.Matcher.Investigator
import {-# SOURCE #-} Arkham.Matcher.Location
import Arkham.Matcher.Value
import {-# SOURCE #-} Arkham.Modifier
import Arkham.Prelude
import Arkham.SkillType
import Arkham.SlotType
import Arkham.Trait (Trait (..))
import Arkham.Trait qualified as Trait
import Control.Lens.Plated (Plated)
import Data.Aeson.TH
import GHC.OverloadedLabels

-- | Relies on game state, can not be used purely
data ExtendedCardMatcher
  = BasicCardMatch CardMatcher
  | WillGoIntoSlot SlotType
  | CardIsBeneathInvestigator Who
  | CardIsBeneathAsset AssetMatcher
  | CardIsAsset AssetMatcher
  | CardWithCopyInHand Who
  | CardIsAttachedToLocation LocationMatcher
  | CardIsAttachedToEncounterCardAt LocationMatcher
  | NotThisCard
  | IsThisCard
  | ControlledBy Who
  | OwnedBy Who
  | InHandOf Who
  | InDeckOf Who
  | InPlayAreaOf Who
  | InDiscardOf Who
  | TopOfDeckOf Who
  | EligibleForCurrentSkillTest
  | SetAsideCardMatch CardMatcher
  | UnderScenarioReferenceMatch CardMatcher
  | VictoryDisplayCardMatch ExtendedCardMatcher
  | HandCardWithDifferentTitleFromAtLeastOneAsset InvestigatorMatcher AssetMatcher CardMatcher
  | ExtendedCardWithOneOf [ExtendedCardMatcher]
  | ExtendedCardMatches [ExtendedCardMatcher]
  | PlayableCardWithCostReduction ActionStatus Int ExtendedCardMatcher
  | PlayableCardWithNoCost ActionStatus ExtendedCardMatcher
  | PlayableCard CostStatus ExtendedCardMatcher
  | PlayableCardWithCriteria ActionStatus CriteriaOverride ExtendedCardMatcher
  | CommittableCard InvestigatorMatcher ExtendedCardMatcher
  | CardWithPerformableAbility AbilityMatcher [ModifierType]
  | CanCancelRevelationEffect ExtendedCardMatcher
  | CanCancelAllEffects ExtendedCardMatcher
  | CardWithoutModifier ModifierType
  | CardIsCommittedBy InvestigatorMatcher
  | ChosenViaCustomization ExtendedCardMatcher
  | PassesCommitRestrictions ExtendedCardMatcher
  | CardWithSharedTraitToAttackingEnemy
  | CardIdentifiedByScenarioMetaKey Key
  deriving stock (Show, Eq, Ord, Data)

instance Plated ExtendedCardMatcher

instance Semigroup ExtendedCardMatcher where
  ExtendedCardMatches xs <> ExtendedCardMatches ys =
    ExtendedCardMatches $ xs <> ys
  ExtendedCardMatches xs <> x = ExtendedCardMatches (x : xs)
  x <> ExtendedCardMatches xs = ExtendedCardMatches (x : xs)
  x <> y = ExtendedCardMatches [x, y]

instance IsLabel "any" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #any

instance IsLabel "guardian" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #guardian

instance IsLabel "seeker" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #seeker

instance IsLabel "rogue" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #rogue

instance IsLabel "mystic" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #mystic

instance IsLabel "survivor" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #survivor

instance IsLabel "ally" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #ally

instance IsLabel "skill" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #skill

instance IsLabel "spell" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #spell

instance IsLabel "item" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #item

instance IsLabel "tome" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #tome

instance IsLabel "talent" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #talent

instance IsLabel "ritual" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #ritual

instance IsLabel "illicit" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #illicit

instance IsLabel "tool" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #tool

instance IsLabel "weapon" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #weapon

instance IsLabel "asset" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #asset

instance IsLabel "event" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #event

instance IsLabel "story" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #story

instance IsLabel "enemy" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #enemy

instance IsLabel "treachery" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #treachery

instance IsLabel "weakness" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #weakness

instance IsLabel "parley" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #parley

instance IsLabel "eligible" ExtendedCardMatcher where
  fromLabel = EligibleForCurrentSkillTest

-- | Only relies on card state, can be used purely with `cardMatch`
data CardMatcher
  = CardWithType CardType
  | CardWithSubType CardSubType
  | CardWithCardCode CardCode
  | CardWithTitle Text
  | CardWithTrait Trait
  | CardWithId CardId
  | CardWithLevel Int
  | CardWithMaxLevel Int
  | CardWithoutKeyword Keyword
  | CardWithKeyword Keyword
  | CardWithClass ClassSymbol
  | CardWithAction Action
  | CardWithoutAction
  | CardWithSkillIcon SkillIcon
  | CardWithOneOf [CardMatcher]
  | CardMatches [CardMatcher]
  | CardWithPrintedLocationConnection LocationSymbol
  | CardWithPrintedLocationSymbol LocationSymbol
  | NotCard CardMatcher
  | IsEncounterCard
  | IsPlayerCard
  | CardIsUnique
  | FastCard
  | NonWeakness
  | SignatureCard
  | WeaknessCard
  | BasicWeaknessCard
  | NonExceptional
  | PermanentCard
  | AnyCard
  | CardWithVengeance
  | CardFillsSlot SlotType
  | CardFillsLessSlots Int SlotType
  | DiscardableCard
  | CardWithRevelation
  | CardOwnedBy InvestigatorId
  | CardFromEncounterSet EncounterSet
  | CardWithOddCost
  | CardWithEvenCost
  | CardWithCost Int
  | CardWithNonZeroCost
  | CardWithOddSkillIcons
  | CardWithEvenSkillIcons
  | CardWithAnySkills
  | CardWithNoSkills
  | CardWithOddNumberOfWordsInTitle
  | CardWithEvenNumberOfWordsInTitle
  | CardWithAvailableCustomization
  | CardTaggedWith Text
  deriving stock (Show, Eq, Ord, Data)

instance Not CardMatcher where
  not_ = NotCard

instance IsString CardMatcher where
  fromString = CardWithTitle . fromString

instance IsLabel "any" CardMatcher where
  fromLabel = AnyCard

instance IsLabel "charm" CardMatcher where
  fromLabel = CardWithTrait Charm

instance IsLabel "talent" CardMatcher where
  fromLabel = CardWithTrait Talent

instance IsLabel "relic" CardMatcher where
  fromLabel = CardWithTrait Relic

instance IsLabel "blessed" CardMatcher where
  fromLabel = CardWithTrait Blessed

instance IsLabel "cursed" CardMatcher where
  fromLabel = CardWithTrait Cursed

instance IsLabel "illicit" CardMatcher where
  fromLabel = CardWithTrait Illicit

instance IsLabel "tactic" CardMatcher where
  fromLabel = CardWithTrait Tactic

instance IsLabel "insight" CardMatcher where
  fromLabel = CardWithTrait Insight

instance IsLabel "tarot" CardMatcher where
  fromLabel = CardWithTrait Tarot

instance IsLabel "tome" CardMatcher where
  fromLabel = CardWithTrait Tome

instance IsLabel "spell" CardMatcher where
  fromLabel = CardWithTrait Spell

instance IsLabel "ritual" CardMatcher where
  fromLabel = CardWithTrait Ritual

instance IsLabel "item" CardMatcher where
  fromLabel = CardWithTrait Item

instance IsLabel "supply" CardMatcher where
  fromLabel = CardWithTrait Trait.Supply

instance IsLabel "tool" CardMatcher where
  fromLabel = CardWithTrait Tool

instance IsLabel "weapon" CardMatcher where
  fromLabel = CardWithTrait Weapon

instance IsLabel "guardian" CardMatcher where
  fromLabel = CardWithClass Guardian

instance IsLabel "mystic" CardMatcher where
  fromLabel = CardWithClass Mystic

instance IsLabel "survivor" CardMatcher where
  fromLabel = CardWithClass Survivor

instance IsLabel "seeker" CardMatcher where
  fromLabel = CardWithClass Seeker

instance IsLabel "rogue" CardMatcher where
  fromLabel = CardWithClass Rogue

instance IsLabel "location" CardMatcher where
  fromLabel = CardWithType LocationType

instance IsLabel "treachery" CardMatcher where
  fromLabel = CardWithType TreacheryType

instance IsLabel "event" CardMatcher where
  fromLabel = CardWithOneOf [CardWithType EventType, CardWithType EncounterEventType]

instance IsLabel "story" CardMatcher where
  fromLabel = CardWithType StoryType

instance IsLabel "skill" CardMatcher where
  fromLabel = CardWithType SkillType

instance IsLabel "enemy" CardMatcher where
  fromLabel = CardWithType EnemyType

instance IsLabel "asset" CardMatcher where
  fromLabel = CardWithOneOf [CardWithType AssetType, CardWithType EncounterAssetType]

instance IsLabel "ally" CardMatcher where
  fromLabel = CardWithTrait Ally

instance IsLabel "parley" CardMatcher where
  fromLabel = CardWithAction #parley

instance IsLabel "weakness" CardMatcher where
  fromLabel = WeaknessCard

isEnemyCard :: CardMatcher -> CardMatcher
isEnemyCard = (#enemy <>)

instance Semigroup CardMatcher where
  AnyCard <> a = a
  a <> AnyCard = a
  CardMatches xs <> CardMatches ys = CardMatches $ xs <> ys
  CardMatches xs <> x = CardMatches (x : xs)
  x <> CardMatches xs = CardMatches (x : xs)
  x <> y = CardMatches [x, y]

instance Monoid CardMatcher where
  mempty = AnyCard

class IsCardMatcher a where
  toCardMatcher :: a -> CardMatcher

instance IsCardMatcher CardMatcher where
  toCardMatcher = id
  {-# INLINE toCardMatcher #-}

instance IsCardMatcher EncounterSet where
  toCardMatcher = CardFromEncounterSet
  {-# INLINE toCardMatcher #-}

instance IsCardMatcher CardType where
  toCardMatcher = CardWithType
  {-# INLINE toCardMatcher #-}

instance IsCardMatcher Trait where
  toCardMatcher = CardWithTrait
  {-# INLINE toCardMatcher #-}

data DiscardedPlayerCardMatcher
  = DiscardedCardMatcher InvestigatorMatcher CardMatcher
  deriving stock (Show, Eq, Ord, Data)

data CardListMatcher
  = LengthIs ValueMatcher
  | HasCard CardMatcher
  | AnyCards
  | DifferentLengthIsAtLeast Int CardMatcher
  | NoCards
  deriving stock (Show, Eq, Ord, Data)

instance IsLabel "item" CardListMatcher where
  fromLabel = HasCard #item

data DeckMatcher
  = EncounterDeck
  | DeckOf InvestigatorMatcher
  | AnyDeck
  | DeckIs DeckSignifier
  | DeckOneOf [DeckMatcher]
  deriving stock (Show, Eq, Ord, Data)

mconcat
  [ deriveJSON defaultOptions ''CardMatcher
  , deriveJSON defaultOptions ''CardListMatcher
  , deriveJSON defaultOptions ''DeckMatcher
  , deriveToJSON defaultOptions ''ExtendedCardMatcher
  ]

instance FromJSON ExtendedCardMatcher where
  parseJSON = withObject "ExtendedCardMatcher" \o -> do
    t :: Text <- o .: "tag"
    case t of
      "AnyCard" -> pure (BasicCardMatch AnyCard)
      "CardMatches" -> BasicCardMatch . CardMatches <$> o .: "contents"
      _ -> $(mkParseJSON defaultOptions ''ExtendedCardMatcher) (Object o)

-- ** Card Helpers **

cardIs :: HasCardCode a => a -> CardMatcher
cardIs = CardWithCardCode . toCardCode

cardsAre :: HasCardCode a => [a] -> CardMatcher
cardsAre = mapOneOf cardIs

fromSets :: [EncounterSet] -> CardMatcher
fromSets = oneOf . map CardFromEncounterSet

instance OneOf ExtendedCardMatcher where
  oneOf = ExtendedCardWithOneOf

instance OneOf CardMatcher where
  oneOf = CardWithOneOf
