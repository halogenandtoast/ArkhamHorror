module Arkham.Matcher.Patterns where

import Arkham.Card.CardType
import Arkham.ClassSymbol
import Arkham.ForMovement
import Arkham.GameValue
import Arkham.Id
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher.Types
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Source
import Arkham.Timing
import Arkham.Trait

-- ** Investigator Patterns **

pattern AtYourLocation :: InvestigatorMatcher
pattern AtYourLocation =
  InvestigatorIfLocation
    (LocationMatchAll [LocationWithInvestigator You, LocationWithModifier CountsAsDifferentLocation])
    You
    (InvestigatorAt (LocationWithInvestigator You))

pattern InvestigatorWithNonEmptyHand :: InvestigatorMatcher
pattern InvestigatorWithNonEmptyHand <- HandWith (LengthIs (GreaterThan (Static 0)))
  where
    InvestigatorWithNonEmptyHand = HandWith (LengthIs (GreaterThan (Static 0)))

pattern InvestigatorWithDiscardableCard :: InvestigatorMatcher
pattern InvestigatorWithDiscardableCard <- HandWith (HasCard DiscardableCard)
  where
    InvestigatorWithDiscardableCard = HandWith (HasCard DiscardableCard)

pattern InvestigatorWithoutActionsRemaining :: InvestigatorMatcher
pattern InvestigatorWithoutActionsRemaining <- InvestigatorWithActionsRemaining (EqualTo (Static 0))
  where
    InvestigatorWithoutActionsRemaining = InvestigatorWithActionsRemaining (EqualTo (Static 0))

pattern InvestigatorWithAnyActionsRemaining :: InvestigatorMatcher
pattern InvestigatorWithAnyActionsRemaining <- InvestigatorWithActionsRemaining (GreaterThan (Static 0))
  where
    InvestigatorWithAnyActionsRemaining = InvestigatorWithActionsRemaining (GreaterThan (Static 0))

pattern InvestigatorWithAnyDoom :: InvestigatorMatcher
pattern InvestigatorWithAnyDoom <- InvestigatorWithDoom (GreaterThan (Static 0))
  where
    InvestigatorWithAnyDoom = InvestigatorWithDoom (GreaterThan (Static 0))

pattern InvestigatorWithAnyDamage :: InvestigatorMatcher
pattern InvestigatorWithAnyDamage <- InvestigatorWithDamage (GreaterThan (Static 0))
  where
    InvestigatorWithAnyDamage = InvestigatorWithDamage (GreaterThan (Static 0))

pattern InvestigatorWithAnyHorror :: InvestigatorMatcher
pattern InvestigatorWithAnyHorror <- InvestigatorWithHorror (GreaterThan (Static 0))
  where
    InvestigatorWithAnyHorror = InvestigatorWithHorror (GreaterThan (Static 0))

pattern InvestigatorWithAnyClues :: InvestigatorMatcher
pattern InvestigatorWithAnyClues <- InvestigatorWithClues (GreaterThan (Static 0))
  where
    InvestigatorWithAnyClues = InvestigatorWithClues (GreaterThan (Static 0))

pattern InvestigatorWithAnyCluesInPool :: InvestigatorMatcher
pattern InvestigatorWithAnyCluesInPool <- InvestigatorWithCluesInPool (GreaterThan (Static 0))
  where
    InvestigatorWithAnyCluesInPool = InvestigatorWithCluesInPool (GreaterThan (Static 0))

pattern InvestigatorWithoutAnyClues :: InvestigatorMatcher
pattern InvestigatorWithoutAnyClues <- InvestigatorWithClues (EqualTo (Static 0))
  where
    InvestigatorWithoutAnyClues = InvestigatorWithClues (EqualTo (Static 0))

pattern InvestigatorWithAnyResources :: InvestigatorMatcher
pattern InvestigatorWithAnyResources <- InvestigatorWithResources (GreaterThan (Static 0))
  where
    InvestigatorWithAnyResources = InvestigatorWithResources (GreaterThan (Static 0))

investigatorWithSpendableResources :: Int -> InvestigatorMatcher
investigatorWithSpendableResources = InvestigatorWithSpendableResources . AtLeast . toGameValue

pattern InvestigatorCanGainResources :: InvestigatorMatcher
pattern InvestigatorCanGainResources <- InvestigatorWithoutModifier CannotGainResources
  where
    InvestigatorCanGainResources = InvestigatorWithoutModifier CannotGainResources

pattern InvestigatorCanSearchDeck :: InvestigatorMatcher
pattern InvestigatorCanSearchDeck <- InvestigatorWithoutModifier CannotManipulateDeck
  where
    InvestigatorCanSearchDeck = InvestigatorWithoutModifier CannotManipulateDeck

-- placeholder in case a modifier prevents spending resources
pattern InvestigatorCanSpendResources :: GameValue -> InvestigatorMatcher
pattern InvestigatorCanSpendResources value <- InvestigatorWithResources (AtLeast value)
  where
    InvestigatorCanSpendResources value = InvestigatorWithResources (AtLeast value)

-- placeholder in case a modifier prevents spending resources
pattern InvestigatorCanSpendClues :: GameValue -> InvestigatorMatcher
pattern InvestigatorCanSpendClues value <-
  InvestigatorMatches
    [InvestigatorWithClues (AtLeast value), InvestigatorWithoutModifier CannotSpendClues]
  where
    InvestigatorCanSpendClues value =
      InvestigatorMatches
        [InvestigatorWithClues (AtLeast value), InvestigatorWithoutModifier CannotSpendClues]

pattern InvestigatorCanDisengage :: InvestigatorMatcher
pattern InvestigatorCanDisengage =
  InvestigatorMatches
    [InvestigatorWithoutModifier CannotDisengageEnemies, InvestigatorEngagedWith AnyEnemy]

pattern InvestigatorCanDiscoverCluesAt
  :: LocationMatcher -> InvestigatorMatcher
pattern InvestigatorCanDiscoverCluesAt locationMatcher =
  InvestigatorMatches
    [ InvestigatorCanDiscoverCluesAtOneOf locationMatcher
      , InvestigatorWithoutModifier CannotDiscoverClues
      ]

pattern InvestigatorCanMove :: InvestigatorMatcher
pattern InvestigatorCanMove <- InvestigatorWithoutModifier CannotMove
  where
    InvestigatorCanMove = InvestigatorWithoutModifier CannotMove

pattern InvestigatorCanHealHorror :: InvestigatorMatcher
pattern InvestigatorCanHealHorror <- InvestigatorWithoutModifier CannotHealHorror
  where
    InvestigatorCanHealHorror = InvestigatorWithoutModifier CannotHealHorror

-- Placeholder
pattern InvestigatorCanHealDamage :: InvestigatorMatcher
pattern InvestigatorCanHealDamage <- InvestigatorWithoutModifier CannotHealDamage
  where
    InvestigatorCanHealDamage = InvestigatorWithoutModifier CannotHealDamage

-- ** Event Patterns **

pattern EventWithAnyDoom :: EventMatcher
pattern EventWithAnyDoom <- EventWithDoom (GreaterThan (Static 0))
  where
    EventWithAnyDoom = EventWithDoom (GreaterThan (Static 0))

-- ** Asset Patterns **

pattern StoryAsset :: AssetMatcher
pattern StoryAsset <- NotAsset AssetNonStory
  where
    StoryAsset = NotAsset AssetNonStory

pattern ItemAsset :: AssetMatcher
pattern ItemAsset <- AssetWithTrait Item
  where
    ItemAsset = AssetWithTrait Item

pattern AllyAsset :: AssetMatcher
pattern AllyAsset <- AssetWithTrait Ally
  where
    AllyAsset = AssetWithTrait Ally

pattern AssetWithAnyDoom :: AssetMatcher
pattern AssetWithAnyDoom <- AssetWithDoom (GreaterThan (Static 0))
  where
    AssetWithAnyDoom = AssetWithDoom (GreaterThan (Static 0))

pattern AssetWithAnyClues :: AssetMatcher
pattern AssetWithAnyClues <- AssetWithClues (GreaterThan (Static 0))
  where
    AssetWithAnyClues = AssetWithClues (GreaterThan (Static 0))

pattern UncontrolledAsset :: AssetMatcher
pattern UncontrolledAsset <- NotAsset ControlledAsset
  where
    UncontrolledAsset = NotAsset ControlledAsset

pattern ControlledAsset :: AssetMatcher
pattern ControlledAsset <- AssetControlledBy Anyone
  where
    ControlledAsset = AssetControlledBy Anyone

-- ** Enemy Patterns **

pattern CanPlaceDoomOnEnemy :: EnemyMatcher
pattern CanPlaceDoomOnEnemy = EnemyWithoutModifier CannotPlaceDoomOnThis

pattern AnyInPlayEnemy :: EnemyMatcher
pattern AnyInPlayEnemy = InPlayEnemy AnyEnemy

pattern IgnoreAloofFightable :: EnemyMatcher
pattern IgnoreAloofFightable = EnemyMatchAll [EnemyAt YourLocation, CanBeAttackedBy You]

pattern EnemyEngagedWithYou :: EnemyMatcher
pattern EnemyEngagedWithYou <- EnemyIsEngagedWith You
  where
    EnemyEngagedWithYou = EnemyIsEngagedWith You

pattern EnemyNotEngagedWithYou :: EnemyMatcher
pattern EnemyNotEngagedWithYou <- NotEnemy (EnemyIsEngagedWith You)
  where
    EnemyNotEngagedWithYou = NotEnemy (EnemyIsEngagedWith You)

pattern AloofEnemy :: EnemyMatcher
pattern AloofEnemy <- EnemyWithKeyword Keyword.Aloof
  where
    AloofEnemy = EnemyWithKeyword Keyword.Aloof

pattern HunterEnemy :: EnemyMatcher
pattern HunterEnemy <- EnemyWithKeyword Keyword.Hunter
  where
    HunterEnemy = EnemyWithKeyword Keyword.Hunter

pattern EliteEnemy :: EnemyMatcher
pattern EliteEnemy <- EnemyWithTrait Elite
  where
    EliteEnemy = EnemyWithTrait Elite

pattern MassiveEnemy :: EnemyMatcher
pattern MassiveEnemy <- EnemyWithKeyword Keyword.Massive
  where
    MassiveEnemy = EnemyWithKeyword Keyword.Massive

pattern NonEliteEnemy :: EnemyMatcher
pattern NonEliteEnemy <- EnemyWithoutTrait Elite
  where
    NonEliteEnemy = EnemyWithoutTrait Elite

pattern EnemyWithAnyClues :: EnemyMatcher
pattern EnemyWithAnyClues <- EnemyWithClues (GreaterThan (Static 0))
  where
    EnemyWithAnyClues = EnemyWithClues (GreaterThan (Static 0))

pattern EnemyWithAnyDoom :: EnemyMatcher
pattern EnemyWithAnyDoom <- EnemyWithDoom (GreaterThan (Static 0))
  where
    EnemyWithAnyDoom = EnemyWithDoom (GreaterThan (Static 0))

pattern EnemyWithoutDoom :: EnemyMatcher
pattern EnemyWithoutDoom <- EnemyWithDoom (EqualTo (Static 0))
  where
    EnemyWithoutDoom = EnemyWithDoom (EqualTo (Static 0))

pattern EnemyWithAnyDamage :: EnemyMatcher
pattern EnemyWithAnyDamage <- EnemyWithDamage (GreaterThan (Static 0))
  where
    EnemyWithAnyDamage = EnemyWithDamage (GreaterThan (Static 0))

-- ** Location Patterns **

pattern LocationCanHaveAttachments :: LocationMatcher
pattern LocationCanHaveAttachments <- LocationWithoutModifier CannotHaveAttachments
  where
    LocationCanHaveAttachments = LocationWithoutModifier CannotHaveAttachments

pattern FarthestLocationFromYou :: LocationMatcher -> LocationMatcher
pattern FarthestLocationFromYou matcher <- FarthestLocationFromInvestigator You matcher
  where
    FarthestLocationFromYou matcher = FarthestLocationFromInvestigator You matcher

pattern YourLocation :: LocationMatcher
pattern YourLocation <- LocationWithInvestigator You
  where
    YourLocation = LocationWithInvestigator You

pattern NotYourLocation :: LocationMatcher
pattern NotYourLocation <- NotLocation YourLocation
  where
    NotYourLocation = NotLocation YourLocation

pattern AccessibleLocation :: LocationMatcher
pattern AccessibleLocation <-
  LocationMatchAll [AccessibleFrom ForMovement YourLocation, CanEnterLocation You]
  where
    AccessibleLocation = LocationMatchAll [AccessibleFrom ForMovement YourLocation, CanEnterLocation You]

pattern CanMoveCluesFromLocation :: LocationMatcher
pattern CanMoveCluesFromLocation <-
  LocationMatchAll [LocationWithoutModifier CannotMoveCluesFromHere, LocationWithAnyClues]
  where
    CanMoveCluesFromLocation = LocationMatchAll [LocationWithoutModifier CannotMoveCluesFromHere, LocationWithAnyClues]

pattern ConnectedLocation :: ForMovement -> LocationMatcher
pattern ConnectedLocation forMovement <- ConnectedFrom forMovement YourLocation
  where
    ConnectedLocation forMovement = ConnectedFrom forMovement YourLocation

pattern LocationWithAnyDoom :: LocationMatcher
pattern LocationWithAnyDoom <- LocationWithDoom (GreaterThan (Static 0))
  where
    LocationWithAnyDoom = LocationWithDoom (GreaterThan (Static 0))

pattern LocationWithAnyClues :: LocationMatcher
pattern LocationWithAnyClues <- LocationWithClues (GreaterThan (Static 0))
  where
    LocationWithAnyClues = LocationWithClues (GreaterThan (Static 0))

pattern LocationWithAnyHorror :: LocationMatcher
pattern LocationWithAnyHorror <-
  LocationWithHorror (GreaterThan (Static 0))
  where
    LocationWithAnyHorror = LocationWithHorror (GreaterThan (Static 0))

pattern LocationWithoutHorror :: LocationMatcher
pattern LocationWithoutHorror <- LocationWithHorror (EqualTo (Static 0))
  where
    LocationWithoutHorror = LocationWithHorror (EqualTo (Static 0))

-- We exclude this pattern because it has to be handled specially
-- pattern LocationWithoutClues :: LocationMatcher
-- pattern LocationWithoutClues <- LocationWithClues (EqualTo (Static 0)) where
--   LocationWithoutClues = LocationWithClues (EqualTo (Static 0))

pattern LocationWithoutDoom :: LocationMatcher
pattern LocationWithoutDoom <- LocationWithDoom (EqualTo (Static 0))
  where
    LocationWithoutDoom = LocationWithDoom (EqualTo (Static 0))

pattern LeadInvestigatorLocation :: LocationMatcher
pattern LeadInvestigatorLocation <-
  LocationWithInvestigator LeadInvestigator
  where
    LeadInvestigatorLocation = LocationWithInvestigator LeadInvestigator

pattern LocationWithoutEnemies :: LocationMatcher
pattern LocationWithoutEnemies <-
  NotLocation (LocationWithEnemy AnyEnemy)
  where
    LocationWithoutEnemies = NotLocation (LocationWithEnemy AnyEnemy)

pattern LocationWithoutInvestigators :: LocationMatcher
pattern LocationWithoutInvestigators <-
  NotLocation (LocationWithInvestigator Anyone)
  where
    LocationWithoutInvestigators = NotLocation (LocationWithInvestigator Anyone)

pattern Unblocked :: LocationMatcher
pattern Unblocked <- LocationWithoutModifier Blocked
  where
    Unblocked = LocationWithoutModifier Blocked

pattern LocationNotOneOf :: [LocationMatcher] -> LocationMatcher
pattern LocationNotOneOf inner = NotLocation (LocationMatchAny inner)

-- ** Card Patterns **

pattern NonSignature :: CardMatcher
pattern NonSignature <- NotCard SignatureCard
  where
    NonSignature = NotCard SignatureCard

pattern NonWeaknessTreachery :: CardMatcher
pattern NonWeaknessTreachery = CardMatches [NonWeakness, CardWithType TreacheryType]

pattern NonPeril :: CardMatcher
pattern NonPeril <- CardWithoutKeyword Keyword.Peril
  where
    NonPeril = CardWithoutKeyword Keyword.Peril

pattern StoryCard :: CardMatcher
pattern StoryCard <- CardWithType StoryType
  where
    StoryCard = CardWithType StoryType

pattern EventCard :: CardMatcher
pattern EventCard <- CardWithOneOf [CardWithType EventType, CardWithType EncounterEventType]
  where
    EventCard = CardWithType EventType

pattern AssetCard :: CardMatcher
pattern AssetCard <- CardWithOneOf [CardWithType AssetType, CardWithType EncounterAssetType]
  where
    AssetCard = CardWithType AssetType

pattern SkillCard :: CardMatcher
pattern SkillCard <- CardWithType SkillType
  where
    SkillCard = CardWithType SkillType

pattern LocationCard :: CardMatcher
pattern LocationCard <- CardWithType LocationType
  where
    LocationCard = CardWithType LocationType

pattern IsAlly :: CardMatcher
pattern IsAlly <- CardMatches [CardWithType AssetType, CardWithTrait Ally]
  where
    IsAlly = CardMatches [CardWithType AssetType, CardWithTrait Ally]

pattern MysticCard :: CardMatcher
pattern MysticCard <- CardWithClass Mystic
  where
    MysticCard = CardWithClass Mystic

pattern PlayerTreachery :: CardMatcher
pattern PlayerTreachery <- CardWithType PlayerTreacheryType
  where
    PlayerTreachery = CardWithType PlayerTreacheryType

-- ** Value Patterns **

atLeast :: Int -> ValueMatcher
atLeast n = AtLeast (Static n)

atMost :: Int -> ValueMatcher
atMost n = AtMost (Static n)

lessThan :: Int -> ValueMatcher
lessThan n = LessThan (Static n)

static :: Int -> ValueMatcher
static n = EqualTo (Static n)

pattern AtLeast :: GameValue -> ValueMatcher
pattern AtLeast n <- GreaterThanOrEqualTo n
  where
    AtLeast n = GreaterThanOrEqualTo n

pattern AtMost :: GameValue -> ValueMatcher
pattern AtMost n <- LessThanOrEqualTo n
  where
    AtMost n = LessThanOrEqualTo n

-- ** Agenda Patterns **

pattern AgendaWithAnyDoom :: AgendaMatcher
pattern AgendaWithAnyDoom <- AgendaWithDoom (GreaterThan (Static 0))
  where
    AgendaWithAnyDoom = AgendaWithDoom (GreaterThan (Static 0))

pattern AgendaWithoutModifier :: ModifierType -> AgendaMatcher
pattern AgendaWithoutModifier m <- NotAgenda (AgendaWithModifier m)
  where
    AgendaWithoutModifier m = NotAgenda (AgendaWithModifier m)

-- ** Treachery Patterns **

pattern TreacheryWithAnyDoom :: TreacheryMatcher
pattern TreacheryWithAnyDoom <- TreacheryWithDoom (GreaterThan (Static 0))
  where
    TreacheryWithAnyDoom = TreacheryWithDoom (GreaterThan (Static 0))

pattern TreacheryWithoutModifier :: ModifierType -> TreacheryMatcher
pattern TreacheryWithoutModifier m <- NotTreachery (TreacheryWithModifier m)
  where
    TreacheryWithoutModifier m = NotTreachery (TreacheryWithModifier m)

-- ** Window Patterns **

pattern InvestigationResult
  :: Timing -> Who -> LocationMatcher -> SkillTestResultMatcher -> WindowMatcher
pattern InvestigationResult timing who where_ result <-
  SkillTestResult timing who (WhileInvestigating where_) result
  where
    InvestigationResult timing who where_ result = SkillTestResult timing who (WhileInvestigating where_) result

pattern SuccessfulInvestigationResult
  :: Timing -> Who -> LocationMatcher -> ValueMatcher -> WindowMatcher
pattern SuccessfulInvestigationResult timing who where_ amount <-
  SkillTestResult timing who (WhileInvestigating where_) (SuccessResult amount)
  where
    SuccessfulInvestigationResult timing who where_ amount = SkillTestResult timing who (WhileInvestigating where_) (SuccessResult amount)

pattern SuccessfulInvestigation :: Timing -> Who -> LocationMatcher -> WindowMatcher
pattern SuccessfulInvestigation timing who where_ <-
  SkillTestResult timing who (WhileInvestigating where_) (SuccessResult AnyValue)
  where
    SuccessfulInvestigation timing who where_ = SkillTestResult timing who (WhileInvestigating where_) (SuccessResult AnyValue)

pattern SuccessfulParley :: Timing -> Who -> WindowMatcher
pattern SuccessfulParley timing who <- SkillTestResult timing who WhileParleying (SuccessResult AnyValue)
  where
    SuccessfulParley timing who = SkillTestResult timing who WhileParleying (SuccessResult AnyValue)

-- * Ability Helpers

pattern BasicInvestigate :: LocationId -> AbilityMatcher
pattern BasicInvestigate lid <- AbilityIs (LocationSource lid) 101
  where
    BasicInvestigate lid = AbilityIs (LocationSource lid) 101
