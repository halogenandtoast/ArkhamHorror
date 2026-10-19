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
import Control.Monad.Fail (fail)
import Data.Aeson (Result (..))
import Data.Aeson.TH
import GHC.OverloadedLabels

data ForPlay = ForPlay | NotForPlay
  deriving stock (Show, Eq, Ord, Data)

instance Plated ForPlay

-- | Relies on game state, can not be used purely
data ExtendedCardMatcher
  = BasicCardMatch CardMatcher
  | NotExtendedCard ExtendedCardMatcher
  | HiddenInHandCard
  | WillGoIntoSlot SlotType
  | CardIsBeneathInvestigator Who
  | CardIsBeneathActDeck
  | CardIsBeneathAct
  | CardSharesTitleWith ExtendedCardMatcher
  | CardIsBeneathAsset AssetMatcher
  | CardIsAsset AssetMatcher
  | CardWithCopyInHand Who
  | CardWithHollowedCopy ExtendedCardMatcher
  | CardIsAttachedToLocation LocationMatcher
  | CardIsAttachedToEncounterCardAt LocationMatcher
  | NotThisCard
  | IsThisCard
  | ControlledBy Who
  | OwnedBy Who
  | InHandOf ForPlay Who
  | {- | A card that, played now, would have no slot to go in. "Assets played
    in excess of available slots gain fast" is a question about the card and
    the table together, which neither a card matcher nor a slot matcher can
    ask on its own.
    -}
    DoesNotFitSlotsOf Who
  | InDeckOf Who
  | InPlayAreaOf Who
  | InDiscardOf Who
  | TopmostOfDiscardOf Who CardMatcher
  | InEncounterDiscard
  | TopOfDeckOf Who
  | EligibleForCurrentSkillTest
  | SetAsideCardMatch CardMatcher
  | UnderScenarioReferenceMatch CardMatcher
  | VictoryDisplayCardMatch ExtendedCardMatcher
  | HandCardWithDifferentTitleFromAtLeastOneAsset InvestigatorMatcher AssetMatcher CardMatcher
  | ExtendedCardWithOneOf [ExtendedCardMatcher]
  | ExtendedCardMatches [ExtendedCardMatcher]
  | {- | Cost of at least @n@ after the cost modifiers already in play, as
    opposed to 'CardWithCostAtLeast', which reads the printed cost. A card with
    no cost at all never matches.

    This is the question a cost-reduction [reaction] has to ask before it offers
    itself: Rod of Animalism (1) reducing a Guard Dog already brought to 0 by
    Motivational Speech does nothing, so it should not prompt.
    -}
    CardWithModifiedCostAtLeast Int
  | PlayableCardWithCostReduction ActionStatus Int ExtendedCardMatcher
  | PlayableCardWithNoCost ActionStatus ExtendedCardMatcher
  | PlayableCard CostStatus ExtendedCardMatcher
  | PlayableCardWithCriteria ActionStatus CriteriaOverride ExtendedCardMatcher
  | CommittableCard InvestigatorMatcher ExtendedCardMatcher
  | CardWithPerformableAbility AbilityMatcher [ModifierType]
  | CanCancelRevelationEffect InvestigatorMatcher ExtendedCardMatcher
  | CanCancelAllEffects InvestigatorMatcher ExtendedCardMatcher
  | CardWithoutModifier ModifierType
  | CardWithModifier ModifierType
  | CardIsCommittedBy InvestigatorMatcher
  | ChosenViaCustomization ExtendedCardMatcher
  | PassesCommitRestrictions ExtendedCardMatcher
  | CardWithSharedTraitToAttackingEnemy
  | CardWithoutUniqueCopyInPlay
  | CardIdentifiedByScenarioMetaKey Key
  | ActiveCard
  | ResolvingCard
  | CardWithVictory
  deriving stock (Show, Eq, Ord, Data)

instance Not ExtendedCardMatcher where
  not_ = NotExtendedCard

instance Plated ExtendedCardMatcher

instance Semigroup ExtendedCardMatcher where
  ExtendedCardMatches xs <> ExtendedCardMatches ys =
    ExtendedCardMatches $ xs <> ys
  ExtendedCardMatches xs <> x = ExtendedCardMatches (x : xs)
  x <> ExtendedCardMatches xs = ExtendedCardMatches (x : xs)
  BasicCardMatch a <> BasicCardMatch b = BasicCardMatch (a <> b)
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

instance IsLabel "hex" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #hex

instance IsLabel "curse" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #curse

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

instance IsLabel "monster" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #monster

instance IsLabel "silverTwilight" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #silverTwilight

instance IsLabel "treachery" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #treachery

instance IsLabel "weakness" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #weakness

instance IsLabel "parley" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #parley

instance IsLabel "investigate" ExtendedCardMatcher where
  fromLabel = BasicCardMatch #investigate

instance IsLabel "eligible" ExtendedCardMatcher where
  fromLabel = EligibleForCurrentSkillTest

-- | Only relies on card state, can be used purely with `cardMatch`
data CardMatcher
  = CardWithType CardType
  | CardWithSubType CardSubType
  | CardWithCardCode CardCode
  | CardWithCardCodeExact CardCodeExact
  | CardWithTitle Text
  | CardWithTitleContaining Text
  | CardWithTrait Trait
  | {- | Has at least one trait, whatever it is. What "choose one of its Traits"
    needs in order not to offer a card whose trait cost could not then be paid.
    -}
    CardWithAnyTrait
  | {- | Every trait the card has is in this list.

    Vacuously true of a card with no traits, which is the right answer: "only if
    all its traits are known" asks nothing of a card that has none. The list is a
    literal, so whoever builds the matcher works out what is allowed -- which for a
    set that comes from the campaign log means building it where the log can be
    read, and then this stays a pure match.
    -}
    CardWithAllTraitsIn [Trait]
  | CardWithId CardId
  | CardWithLevel Int
  | CardWithMaxLevel Int
  | {- | Matches a card whose printed enemy health (fixed or per-investigator) is
    at most the given value. Cards with no fixed printed health (X, *, or none)
    never match. Requires the Player Count as the first Int to be able to calculate purely
    -}
    CardWithMaxPrintedHealth Int Int
  | CardWithoutKeyword Keyword
  | CardWithKeyword Keyword
  | CardWithConcealed
  | CardWithClass ClassSymbol
  | CardWithAction Action
  | CardWithoutAction
  | CardIsStoryAsset
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
  | SingleSidedCard
  | -- | Printed cost of at least @n@; a card with no cost never matches.
    CardWithCostAtLeast Int
  deriving stock (Show, Eq, Ord, Data)

{- | A matcher over a /search/ rather than over a card: it reads the matcher a
search was handed and answers what that search is looking for.

An ability worded "when a card would search the encounter deck to find an
enemy" asks about the search's intent, not about what it turns up -- it has to
answer the same way when the deck holds no enemy at all -- and that intent is
the 'CardMatcher' the search was given.

Only positive positions count: a search that explicitly excludes enemies does
not answer 'SearchingFor' @EnemyType@, and a disjunction answers yes only when
every branch does, because otherwise the search could find something else.
-}
data SearchMatcher
  = AnySearch
  | -- | The search can only find cards of this type.
    SearchingFor CardType
  | -- | The search can only find cards with this trait.
    SearchingForTrait Trait
  | -- | The search's matcher demands this constraint of everything it can find.
    SearchingForMatching CardMatcher
  | SearchMatches [SearchMatcher]
  | SearchMatchesAny [SearchMatcher]
  | NotSearch SearchMatcher
  deriving stock (Show, Eq, Ord, Data)

{- | Answer a 'SearchMatcher' against the card matcher a search was given. Pure:
it reads the matcher, never the game.
-}
searchMatches :: SearchMatcher -> CardMatcher -> Bool
searchMatches = \case
  AnySearch -> const True
  SearchingFor ty -> cardMatcherRequires (CardWithType ty)
  SearchingForTrait t -> cardMatcherRequires (CardWithTrait t)
  SearchingForMatching needle -> cardMatcherRequires needle
  SearchMatches xs -> \m -> all (`searchMatches` m) xs
  SearchMatchesAny xs -> \m -> any (`searchMatches` m) xs
  NotSearch x -> not . searchMatches x

{- | Does this card matcher demand @needle@ of every card it can match? A
conjunction demands it when any conjunct does, a disjunction only when every
branch does, and a negation never does.
-}
cardMatcherRequires :: CardMatcher -> CardMatcher -> Bool
cardMatcherRequires needle = go
 where
  go m
    | m == needle = True
    | otherwise = case m of
        CardMatches xs -> any go xs
        CardWithOneOf xs -> not (null xs) && all go xs
        _ -> False

{- | Answer a 'SearchMatcher' against an extended matcher. The extended
constructors ask about game state, so they carry no intent a 'SearchMatcher' can
read; the basic constraints inside do.
-}
searchMatchesExtended :: SearchMatcher -> ExtendedCardMatcher -> Bool
searchMatchesExtended sm = searchMatches sm . basicConstraints
 where
  basicConstraints = \case
    BasicCardMatch m -> m
    ExtendedCardMatches xs -> fold (map basicConstraints xs)
    ExtendedCardWithOneOf xs -> case map basicConstraints xs of
      [] -> AnyCard
      ys -> CardWithOneOf ys
    _ -> AnyCard

instance Not SearchMatcher where
  not_ = NotSearch

instance Semigroup SearchMatcher where
  AnySearch <> a = a
  a <> AnySearch = a
  SearchMatches xs <> SearchMatches ys = SearchMatches $ xs <> ys
  SearchMatches xs <> x = SearchMatches (xs <> [x])
  x <> SearchMatches xs = SearchMatches (x : xs)
  x <> y = SearchMatches [x, y]

instance Monoid SearchMatcher where
  mempty = AnySearch

instance IsLabel "enemy" SearchMatcher where
  fromLabel = SearchingFor EnemyType

instance IsLabel "treachery" SearchMatcher where
  fromLabel = SearchingFor TreacheryType

instance IsLabel "location" SearchMatcher where
  fromLabel = SearchingFor LocationType

instance IsLabel "any" SearchMatcher where
  fromLabel = AnySearch

instance Not CardMatcher where
  not_ = NotCard

instance IsString CardMatcher where
  fromString = CardWithTitle . fromString

instance IsLabel "any" CardMatcher where
  fromLabel = AnyCard

instance IsLabel "unique" CardMatcher where
  fromLabel = CardIsUnique

instance IsLabel "charm" CardMatcher where
  fromLabel = CardWithTrait Charm

instance IsLabel "upgrade" CardMatcher where
  fromLabel = CardWithTrait Upgrade

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

instance IsLabel "trick" CardMatcher where
  fromLabel = CardWithTrait Trick

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

instance IsLabel "firearm" CardMatcher where
  fromLabel = CardWithTrait Firearm

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

instance IsLabel "cultist" CardMatcher where
  fromLabel = CardWithTrait Cultist

instance IsLabel "monster" CardMatcher where
  fromLabel = CardWithTrait Monster

instance IsLabel "silverTwilight" CardMatcher where
  fromLabel = CardWithTrait SilverTwilight

instance IsLabel "asset" CardMatcher where
  fromLabel = CardWithOneOf [CardWithType AssetType, CardWithType EncounterAssetType]

instance IsLabel "ally" CardMatcher where
  fromLabel = CardWithTrait Ally

instance IsLabel "hex" CardMatcher where
  fromLabel = CardWithTrait Hex

instance IsLabel "curse" CardMatcher where
  fromLabel = CardWithTrait Curse

instance IsLabel "parley" CardMatcher where
  fromLabel = CardWithAction #parley

instance IsLabel "weakness" CardMatcher where
  fromLabel = WeaknessCard

instance IsLabel "investigate" CardMatcher where
  fromLabel = CardWithAction #investigate

isEnemyCard :: CardMatcher -> CardMatcher
isEnemyCard = (#enemy <>)

cardMatcher_ :: CardMatcher -> CardMatcher
cardMatcher_ = id

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
  | NotCards CardListMatcher
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
  , deriveJSON defaultOptions ''SearchMatcher
  , deriveJSON defaultOptions ''CardListMatcher
  , deriveJSON defaultOptions ''DeckMatcher
  , deriveJSON defaultOptions ''ForPlay
  , deriveToJSON defaultOptions ''ExtendedCardMatcher
  ]

instance FromJSON ExtendedCardMatcher where
  parseJSON = withObject "ExtendedCardMatcher" \o -> do
    t :: Text <- o .: "tag"
    case t of
      "CardWithHollowedCopy" -> do
        contents <- o .:? "contents" .!= BasicCardMatch AnyCard
        pure $ CardWithHollowedCopy contents
      "AnyCard" -> pure (BasicCardMatch AnyCard)
      "CardMatches" -> BasicCardMatch . CardMatches <$> o .: "contents"
      "InHandOf" -> do
        contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
        pure $ case contents of
          Left iid -> InHandOf ForPlay iid
          Right (forPlay, iid) -> InHandOf forPlay iid
      "CanCancelRevelationEffect" -> do
        contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
        case contents of
          Left matcher -> do
            case fromJSON (object ["tag" .= ("Anyone" :: Text)]) of
              Success imatcher -> pure $ CanCancelRevelationEffect imatcher matcher
              Error err -> fail err
          Right (imatcher, matcher) -> pure $ CanCancelRevelationEffect imatcher matcher
      "CanCancelAllEffects" -> do
        contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
        case contents of
          Left matcher -> do
            case fromJSON (object ["tag" .= ("Anyone" :: Text)]) of
              Success imatcher -> pure $ CanCancelAllEffects imatcher matcher
              Error err -> fail err
          Right (imatcher, matcher) -> pure $ CanCancelAllEffects imatcher matcher
      {- A bare 'CardMatcher' decodes as the 'BasicCardMatch' it means. Saved
      games hold matchers written when a field took the plain type -- a window's
      payload, say -- and the two share no tag names, so trying the extended
      constructors first keeps those exact. -}
      _ ->
        $(mkParseJSON defaultOptions ''ExtendedCardMatcher) (Object o)
          <|> (BasicCardMatch <$> parseJSON (Object o))

-- ** Card Helpers **

cardIs :: HasCardCode a => a -> CardMatcher
cardIs = CardWithCardCode . toCardCode

cardIsExact :: HasCardCode a => a -> CardMatcher
cardIsExact = CardWithCardCodeExact . CardCodeExact . toCardCode

cardsAre :: HasCardCode a => [a] -> CardMatcher
cardsAre = mapOneOf cardIs

fromSets :: [EncounterSet] -> CardMatcher
fromSets = oneOf . map CardFromEncounterSet

instance OneOf ExtendedCardMatcher where
  oneOf = ExtendedCardWithOneOf

instance OneOf CardMatcher where
  oneOf = CardWithOneOf
