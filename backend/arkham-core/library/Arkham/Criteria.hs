{-# LANGUAGE TemplateHaskell #-}

module Arkham.Criteria (
  module Arkham.Criteria,
  module Arkham.Criteria.Override,
) where

import Arkham.Prelude

import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Cost.Status
import Arkham.Criteria.Override
import Arkham.Direction
import Arkham.GameValue
import Arkham.Matcher
import {-# SOURCE #-} Arkham.Modifier
import Arkham.Scenario.Deck
import Arkham.ScenarioLogKey
import Arkham.Trait
import Arkham.Zone
import Data.Aeson.TH

data DiscardSignifier = AnyPlayerDiscard | DiscardOf Who
  deriving stock (Show, Eq, Ord, Data)

pattern AnyHorrorOnThis :: Criterion
pattern AnyHorrorOnThis <- HorrorOnThis (GreaterThan (Static 0))
  where
    AnyHorrorOnThis = HorrorOnThis (GreaterThan (Static 0))

pattern AnyDamageOnThis :: Criterion
pattern AnyDamageOnThis <- DamageOnThis (GreaterThan (Static 0))
  where
    AnyDamageOnThis = DamageOnThis (GreaterThan (Static 0))

pattern NoCluesOnThis :: Criterion
pattern NoCluesOnThis <- CluesOnThis (EqualTo (Static 0))
  where
    NoCluesOnThis = CluesOnThis (EqualTo (Static 0))

pattern CanGainResources :: Criterion
pattern CanGainResources <- Negate (SelfHasModifier CannotGainResources)
  where
    CanGainResources = Negate (SelfHasModifier CannotGainResources)

pattern CanDealDamage :: Criterion
pattern CanDealDamage <- Negate (SelfHasModifier CannotDealDamage)
  where
    CanDealDamage = Negate (SelfHasModifier CannotGainResources)

pattern CanDiscoverCluesAt :: LocationMatcher -> Criterion
pattern CanDiscoverCluesAt locationMatcher =
  InvestigatorExists (InvestigatorMatches [You, InvestigatorCanDiscoverCluesAt locationMatcher])

-- TODO: This is too close in name to CanDiscoverCluesAt, need to determine if CanDiscoverCluesAt needs to exist
pattern AbleToDiscoverCluesAt :: LocationMatcher -> Criterion
pattern AbleToDiscoverCluesAt locationMatcher =
  Criteria [OnLocation LocationWithAnyClues, CanDiscoverCluesAt locationMatcher]

pattern CanTakeControlOfClues :: Criterion
pattern CanTakeControlOfClues <-
  Negate (SelfHasModifier CannotTakeControlOfClues)
  where
    CanTakeControlOfClues = Negate (SelfHasModifier CannotTakeControlOfClues)

pattern CanDrawCards :: Criterion
pattern CanDrawCards <- Negate (SelfHasModifier CannotDrawCards)
  where
    CanDrawCards =
      Criteria [Negate (SelfHasModifier CannotDrawCards), CanManipulateDeck]

pattern CanSearchDeck :: Criterion
pattern CanSearchDeck <- CanManipulateDeck
  where
    CanSearchDeck = CanManipulateDeck

pattern CanShuffleDeck :: Criterion
pattern CanShuffleDeck <- CanManipulateDeck
  where
    CanShuffleDeck = CanManipulateDeck

pattern CanManipulateDeck :: Criterion
pattern CanManipulateDeck <-
  Negate (SelfHasModifier CannotManipulateDeck)
  where
    CanManipulateDeck = Negate (SelfHasModifier CannotManipulateDeck)

data Criterion
  = AssetExists AssetMatcher
  | EventExists EventMatcher
  | ExcludeWindowAssetExists AssetMatcher
  | AgendaExists AgendaMatcher
  | ActExists ActMatcher
  | InYourHand
  | DoomCountIs ValueMatcher
  | OnAct Int
  | CardExists CardMatcher
  | CardInDiscard DiscardSignifier CardMatcher
  | ChargesOnThis ValueMatcher
  | ClueOnLocation
  | CluesOnThis ValueMatcher
  | DuringSkillTest SkillTestMatcher
  | DuringTurn InvestigatorMatcher
  | EnemyCriteria EnemyCriterion
  | ExtendedCardExists ExtendedCardMatcher
  | CommitedCardsMatch CardListMatcher
  | FirstAction
  | HasSupply Supply
  | Here
  | HorrorOnThis ValueMatcher
  | DamageOnThis ValueMatcher
  | InThreatAreaOf InvestigatorMatcher
  | InVictoryDisplay CardMatcher ValueMatcher
  | InvestigatorExists InvestigatorMatcher
  | InvestigatorIsAlone
  | InvestigatorsHaveSpendableClues ValueMatcher
  | LocationExists LocationMatcher
  | LocationCount Int LocationMatcher
  | ExtendedCardCount Int ExtendedCardMatcher
  | AllUndefeatedInvestigatorsResigned
  | EachUndefeatedInvestigator InvestigatorMatcher
  | OnLocation LocationMatcher
  | AllLocationsMatch LocationMatcher LocationMatcher
  | CanAffordCostIncrease Int
  | OnSameLocation
  | OwnCardWithDoom
  | ControlsThis -- really controls this
  | PlayableCardExists CostStatus ExtendedCardMatcher
  | PlayableCardExistsWithCostReduction Int ExtendedCardMatcher
  | ResourcesOnThis ValueMatcher
  | ResourcesOnLocation Where ValueMatcher
  | ReturnableCardInDiscard DiscardSignifier [Trait]
  | PlayableCardInDiscard DiscardSignifier CardMatcher
  | ScenarioCardHasResignAbility
  | ScenarioDeckWithCard ScenarioDeckKey
  | EncounterDeckIsNotEmpty
  | Self
  | SetAsideCardExists CardMatcher
  | OutOfPlayEnemyExists OutOfPlayZone EnemyMatcher
  | TreacheryExists TreacheryMatcher
  | Uncontrolled
  | SelfHasModifier ModifierType
  | ValueIs Int ValueMatcher
  | UnderneathCardCount ValueMatcher UnderZone CardMatcher
  | Remembered ScenarioLogKey
  | RememberedAtLeast GameValue [ScenarioLogKey]
  | ActionCanBeUndone
  | DuringPhase PhaseMatcher
  | ChaosTokenCountIs ChaosTokenMatcher ValueMatcher
  | CanMoveThis GridDirection
  | -- Special Criterion
    AtLeastNCriteriaMet Int [Criterion]
  | Criteria [Criterion]
  | AnyCriterion [Criterion]
  | NoRestriction
  | Never
  | Negate Criterion
  | DuringAction
  | AffectedByTarot
  deriving stock (Show, Eq, Ord, Data)

enemyExists :: EnemyMatcher -> Criterion
enemyExists = EnemyCriteria . EnemyExists

anyInvestigatorExists :: [InvestigatorMatcher] -> Criterion
anyInvestigatorExists = InvestigatorExists . AnyInvestigator

class Exists a where
  exists :: a -> Criterion

instance Exists InvestigatorMatcher where
  exists = InvestigatorExists

instance Exists AssetMatcher where
  exists = AssetExists

instance Exists LocationMatcher where
  exists = LocationExists

instance Exists EnemyMatcher where
  exists = enemyExists

instance Exists ExtendedCardMatcher where
  exists = ExtendedCardExists

pattern CanPlaceDoomOnThis :: Criterion
pattern CanPlaceDoomOnThis <- Negate (SelfHasModifier CannotPlaceDoomOnThis)
  where
    CanPlaceDoomOnThis = Negate (SelfHasModifier CannotPlaceDoomOnThis)

instance Semigroup Criterion where
  Never <> _ = Never
  _ <> Never = Never
  NoRestriction <> x = x
  x <> NoRestriction = x
  Criteria xs <> Criteria ys = Criteria $ xs <> ys
  Criteria xs <> x = Criteria $ x : xs
  x <> Criteria xs = Criteria $ x : xs
  x <> y = Criteria [x, y]

instance Monoid Criterion where
  mempty = NoRestriction

data EnemyCriterion
  = EnemyExists EnemyMatcher
  | NotAttackingEnemy
  | EnemyExistsAtAttachedLocation EnemyMatcher
  | ThisEnemy EnemyMatcher
  | EnemyMatchesCriteria [EnemyCriterion]
  deriving stock (Show, Eq, Ord, Data)

instance Semigroup EnemyCriterion where
  EnemyMatchesCriteria xs <> EnemyMatchesCriteria ys =
    EnemyMatchesCriteria $ xs <> ys
  EnemyMatchesCriteria xs <> x = EnemyMatchesCriteria $ x : xs
  x <> EnemyMatchesCriteria xs = EnemyMatchesCriteria $ x : xs
  x <> y = EnemyMatchesCriteria [x, y]

data UnderZone = UnderActDeck | UnderAgendaDeck | UnderZones [UnderZone]
  deriving stock (Show, Eq, Ord, Data)

instance Semigroup UnderZone where
  UnderZones xs <> UnderZones ys = UnderZones $ xs <> ys
  UnderZones xs <> y = UnderZones $ xs <> [y]
  x <> UnderZones ys = UnderZones $ x : ys
  x <> y = UnderZones [x, y]

$(deriveJSON defaultOptions ''DiscardSignifier)
$(deriveJSON defaultOptions ''UnderZone)
$(deriveJSON defaultOptions ''EnemyCriterion)
$(deriveJSON defaultOptions ''Criterion)
