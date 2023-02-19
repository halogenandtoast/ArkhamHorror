module Arkham.Matcher.Patterns where

import Arkham.Card.CardType
import Arkham.ClassSymbol
import Arkham.GameValue
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher.Types
import Arkham.Modifier
import Arkham.Trait

-- ** Investigator Patterns **

pattern InvestigatorWithoutActionsRemaining :: InvestigatorMatcher
pattern InvestigatorWithoutActionsRemaining <-
  InvestigatorWithActionsRemaining (EqualTo (Static 0)) where
  InvestigatorWithoutActionsRemaining =
    InvestigatorWithActionsRemaining (EqualTo (Static 0))

pattern InvestigatorWithAnyActionsRemaining :: InvestigatorMatcher
pattern InvestigatorWithAnyActionsRemaining <-
  InvestigatorWithActionsRemaining (GreaterThan (Static 0)) where
  InvestigatorWithAnyActionsRemaining =
    InvestigatorWithActionsRemaining (GreaterThan (Static 0))

pattern InvestigatorWithAnyDoom :: InvestigatorMatcher
pattern InvestigatorWithAnyDoom <-
  InvestigatorWithDoom (GreaterThan (Static 0)) where
  InvestigatorWithAnyDoom = InvestigatorWithDoom (GreaterThan (Static 0))

pattern InvestigatorWithAnyDamage :: InvestigatorMatcher
pattern InvestigatorWithAnyDamage <-
  InvestigatorWithDamage (GreaterThan (Static 0)) where
  InvestigatorWithAnyDamage = InvestigatorWithDamage (GreaterThan (Static 0))

pattern InvestigatorWithAnyHorror :: InvestigatorMatcher
pattern InvestigatorWithAnyHorror <-
  InvestigatorWithHorror (GreaterThan (Static 0)) where
  InvestigatorWithAnyHorror = InvestigatorWithHorror (GreaterThan (Static 0))

pattern InvestigatorWithAnyClues :: InvestigatorMatcher
pattern InvestigatorWithAnyClues <-
  InvestigatorWithClues (GreaterThan (Static 0)) where
  InvestigatorWithAnyClues = InvestigatorWithClues (GreaterThan (Static 0))

pattern InvestigatorWithoutAnyClues :: InvestigatorMatcher
pattern InvestigatorWithoutAnyClues <-
  InvestigatorWithClues (EqualTo (Static 0)) where
  InvestigatorWithoutAnyClues = InvestigatorWithClues (EqualTo (Static 0))

pattern InvestigatorWithAnyResources :: InvestigatorMatcher
pattern InvestigatorWithAnyResources <-
  InvestigatorWithResources (GreaterThan (Static 0)) where
  InvestigatorWithAnyResources =
    InvestigatorWithResources (GreaterThan (Static 0))

pattern InvestigatorCanGainResources :: InvestigatorMatcher
pattern InvestigatorCanGainResources <-
  InvestigatorWithoutModifier CannotGainResources where
  InvestigatorCanGainResources =
    InvestigatorWithoutModifier CannotGainResources

-- placeholder in case a modifier prevents spending resources
pattern InvestigatorCanSpendResources :: GameValue -> InvestigatorMatcher
pattern InvestigatorCanSpendResources value <-
  InvestigatorWithResources (AtLeast value) where
  InvestigatorCanSpendResources value =
    InvestigatorWithResources (AtLeast value)

-- placeholder in case a modifier prevents spending resources
pattern InvestigatorCanSpendClues :: GameValue -> InvestigatorMatcher
pattern InvestigatorCanSpendClues value <-
  InvestigatorMatches [InvestigatorWithClues (AtLeast value), InvestigatorWithoutModifier CannotSpendClues]

pattern InvestigatorCanDisengage :: InvestigatorMatcher
pattern InvestigatorCanDisengage =
  InvestigatorMatches [InvestigatorWithoutModifier CannotDisengageEnemies, InvestigatorEngagedWith AnyEnemy]

pattern InvestigatorCanDiscoverCluesAt
  :: LocationMatcher -> InvestigatorMatcher
pattern InvestigatorCanDiscoverCluesAt locationMatcher =
  InvestigatorMatches [InvestigatorCanDiscoverCluesAtOneOf locationMatcher, InvestigatorWithoutModifier CannotDiscoverClues]

pattern InvestigatorCanMove :: InvestigatorMatcher
pattern InvestigatorCanMove <- InvestigatorWithoutModifier CannotMove where
  InvestigatorCanMove = InvestigatorWithoutModifier CannotMove

pattern InvestigatorCanHealHorror :: InvestigatorMatcher
pattern InvestigatorCanHealHorror <-
  InvestigatorWithoutModifier CannotHealHorror where
  InvestigatorCanHealHorror = InvestigatorWithoutModifier CannotHealHorror

-- Placeholder
pattern InvestigatorCanHealDamage :: InvestigatorMatcher
pattern InvestigatorCanHealDamage <- Anyone where
  InvestigatorCanHealDamage = Anyone

-- ** Event Patterns **

pattern EventWithAnyDoom :: EventMatcher
pattern EventWithAnyDoom <- EventWithDoom (GreaterThan (Static 0)) where
  EventWithAnyDoom = EventWithDoom (GreaterThan (Static 0))

-- ** Asset Patterns **

pattern ItemAsset :: AssetMatcher
pattern ItemAsset <- AssetWithTrait Item where
  ItemAsset = AssetWithTrait Item

pattern AllyAsset :: AssetMatcher
pattern AllyAsset <- AssetWithTrait Ally where
  AllyAsset = AssetWithTrait Ally

pattern AssetWithAnyDoom :: AssetMatcher
pattern AssetWithAnyDoom <- AssetWithDoom (GreaterThan (Static 0)) where
  AssetWithAnyDoom = AssetWithDoom (GreaterThan (Static 0))

pattern UncontrolledAsset :: AssetMatcher
pattern UncontrolledAsset <- NotAsset ControlledAsset where
  UncontrolledAsset = NotAsset ControlledAsset

pattern ControlledAsset :: AssetMatcher
pattern ControlledAsset <- AssetControlledBy Anyone where
  ControlledAsset = AssetControlledBy Anyone

-- ** Enemy Patterns **

pattern AloofEnemy :: EnemyMatcher
pattern AloofEnemy <- EnemyWithKeyword Keyword.Aloof where
  AloofEnemy = EnemyWithKeyword Keyword.Aloof

pattern HunterEnemy :: EnemyMatcher
pattern HunterEnemy <- EnemyWithKeyword Keyword.Hunter where
  HunterEnemy = EnemyWithKeyword Keyword.Hunter

pattern EliteEnemy :: EnemyMatcher
pattern EliteEnemy <- EnemyWithTrait Elite where
  EliteEnemy = EnemyWithTrait Elite

pattern MassiveEnemy :: EnemyMatcher
pattern MassiveEnemy <- EnemyWithKeyword Keyword.Massive where
  MassiveEnemy = EnemyWithKeyword Keyword.Massive

pattern NonEliteEnemy :: EnemyMatcher
pattern NonEliteEnemy <- EnemyWithoutTrait Elite where
  NonEliteEnemy = EnemyWithoutTrait Elite

pattern EnemyWithAnyClues :: EnemyMatcher
pattern EnemyWithAnyClues <- EnemyWithClues (GreaterThan (Static 0)) where
  EnemyWithAnyClues = EnemyWithClues (GreaterThan (Static 0))

pattern EnemyWithAnyDoom :: EnemyMatcher
pattern EnemyWithAnyDoom <- EnemyWithDoom (GreaterThan (Static 0)) where
  EnemyWithAnyDoom = EnemyWithDoom (GreaterThan (Static 0))

pattern EnemyWithAnyDamage :: EnemyMatcher
pattern EnemyWithAnyDamage <- EnemyWithDamage (GreaterThan (Static 0)) where
  EnemyWithAnyDamage = EnemyWithDamage (GreaterThan (Static 0))

-- ** Location Patterns **

pattern LocationWithAnyDoom :: LocationMatcher
pattern LocationWithAnyDoom <- LocationWithDoom (GreaterThan (Static 0)) where
  LocationWithAnyDoom = LocationWithDoom (GreaterThan (Static 0))

pattern LocationWithAnyClues :: LocationMatcher
pattern LocationWithAnyClues <-
  LocationWithClues (GreaterThan (Static 0)) where
  LocationWithAnyClues = LocationWithClues (GreaterThan (Static 0))

pattern LocationWithAnyHorror :: LocationMatcher
pattern LocationWithAnyHorror <-
  LocationWithHorror (GreaterThan (Static 0)) where
  LocationWithAnyHorror = LocationWithHorror (GreaterThan (Static 0))

pattern LocationWithoutHorror :: LocationMatcher
pattern LocationWithoutHorror <- LocationWithHorror (EqualTo (Static 0)) where
  LocationWithoutHorror = LocationWithHorror (EqualTo (Static 0))

-- We exclude this pattern because it has to be handled specially
-- pattern LocationWithoutClues :: LocationMatcher
-- pattern LocationWithoutClues <- LocationWithClues (EqualTo (Static 0)) where
--   LocationWithoutClues = LocationWithClues (EqualTo (Static 0))

pattern LocationWithoutDoom :: LocationMatcher
pattern LocationWithoutDoom <- LocationWithDoom (EqualTo (Static 0)) where
  LocationWithoutDoom = LocationWithDoom (EqualTo (Static 0))

pattern LeadInvestigatorLocation :: LocationMatcher
pattern LeadInvestigatorLocation <-
  LocationWithInvestigator LeadInvestigator where
  LeadInvestigatorLocation = LocationWithInvestigator LeadInvestigator

pattern LocationWithoutEnemies :: LocationMatcher
pattern LocationWithoutEnemies <-
  NotLocation (LocationWithEnemy AnyEnemy) where
  LocationWithoutEnemies = NotLocation (LocationWithEnemy AnyEnemy)

pattern LocationWithoutInvestigators :: LocationMatcher
pattern LocationWithoutInvestigators <-
  NotLocation (LocationWithInvestigator Anyone) where
  LocationWithoutInvestigators = NotLocation (LocationWithInvestigator Anyone)

pattern Unblocked :: LocationMatcher
pattern Unblocked <- LocationWithoutModifier Blocked where
  Unblocked = LocationWithoutModifier Blocked

pattern LocationOfThis :: LocationMatcher
pattern LocationOfThis <- SameLocation where
  LocationOfThis = SameLocation

-- ** Card Patterns **

pattern NonWeaknessTreachery :: CardMatcher
pattern NonWeaknessTreachery =
  CardMatches [NonWeakness, CardWithType TreacheryType]

pattern NonPeril :: CardMatcher
pattern NonPeril <- CardWithoutKeyword Keyword.Peril where
  NonPeril = CardWithoutKeyword Keyword.Peril

pattern EventCard :: CardMatcher
pattern EventCard <- CardWithType EventType where
  EventCard = CardWithType EventType

pattern AssetCard :: CardMatcher
pattern AssetCard <- CardWithType AssetType where
  AssetCard = CardWithType AssetType

pattern SkillCard :: CardMatcher
pattern SkillCard <- CardWithType SkillType where
  SkillCard = CardWithType SkillType

pattern LocationCard :: CardMatcher
pattern LocationCard <- CardWithType LocationType where
  LocationCard = CardWithType LocationType

pattern IsAlly :: CardMatcher
pattern IsAlly <-
  CardMatches [CardWithType AssetType, CardWithTrait Ally] where
  IsAlly = CardMatches [CardWithType AssetType, CardWithTrait Ally]

pattern MysticCard :: CardMatcher
pattern MysticCard <- CardWithClass Mystic where
  MysticCard = CardWithClass Mystic

pattern PlayerTreachery :: CardMatcher
pattern PlayerTreachery <- CardWithType PlayerTreacheryType where
  PlayerTreachery = CardWithType PlayerTreacheryType

-- ** Value Patterns **

pattern AtLeast :: GameValue -> ValueMatcher
pattern AtLeast n <- GreaterThanOrEqualTo n where
  AtLeast n = GreaterThanOrEqualTo n

pattern AtMost :: GameValue -> ValueMatcher
pattern AtMost n <- LessThanOrEqualTo n where
  AtMost n = LessThanOrEqualTo n

-- ** Agenda Patterns **

pattern AgendaWithAnyDoom :: AgendaMatcher
pattern AgendaWithAnyDoom <- AgendaWithDoom (GreaterThan (Static 0)) where
  AgendaWithAnyDoom = AgendaWithDoom (GreaterThan (Static 0))

-- ** Treachery Patterns **

pattern TreacheryWithAnyDoom :: TreacheryMatcher
pattern TreacheryWithAnyDoom <-
  TreacheryWithDoom (GreaterThan (Static 0)) where
  TreacheryWithAnyDoom = TreacheryWithDoom (GreaterThan (Static 0))
