module Arkham.Criteria where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Scenario.Deck
import Arkham.ScenarioLogKey
import Arkham.Trait

data DiscardSignifier = AnyPlayerDiscard | DiscardOf Who
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

pattern AnyHorrorOnThis :: Criterion
pattern AnyHorrorOnThis <- HorrorOnThis (GreaterThan (Static 0)) where
  AnyHorrorOnThis = HorrorOnThis (GreaterThan (Static 0))

pattern AnyDamageOnThis :: Criterion
pattern AnyDamageOnThis <- DamageOnThis (GreaterThan (Static 0)) where
  AnyDamageOnThis = DamageOnThis (GreaterThan (Static 0))

pattern NoCluesOnThis :: Criterion
pattern NoCluesOnThis <- CluesOnThis (EqualTo (Static 0)) where
  NoCluesOnThis = CluesOnThis (EqualTo (Static 0))

pattern CanGainResources :: Criterion
pattern CanGainResources <- Negate (SelfHasModifier CannotGainResources) where
  CanGainResources = Negate (SelfHasModifier CannotGainResources)

pattern CanDiscoverClues :: Criterion
pattern CanDiscoverClues <- Negate (SelfHasModifier CannotDiscoverClues) where
  CanDiscoverClues = Negate (SelfHasModifier CannotDiscoverClues)

pattern CanTakeControlOfClues :: Criterion
pattern CanTakeControlOfClues <-
  Negate (SelfHasModifier CannotTakeControlOfClues) where
  CanTakeControlOfClues = Negate (SelfHasModifier CannotTakeControlOfClues)

pattern CanDrawCards :: Criterion
pattern CanDrawCards <- Negate (SelfHasModifier CannotDrawCards) where
  CanDrawCards =
    Criteria [Negate (SelfHasModifier CannotDrawCards), CanManipulateDeck]

pattern CanSearchDeck :: Criterion
pattern CanSearchDeck <- CanManipulateDeck where
  CanSearchDeck = CanManipulateDeck

pattern CanShuffleDeck :: Criterion
pattern CanShuffleDeck <- CanManipulateDeck where
  CanShuffleDeck = CanManipulateDeck

pattern CanManipulateDeck :: Criterion
pattern CanManipulateDeck <-
  Negate (SelfHasModifier CannotManipulateDeck) where
  CanManipulateDeck = Negate (SelfHasModifier CannotManipulateDeck)

data Criterion
  = AssetExists AssetMatcher
  | AgendaExists AgendaMatcher
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
  | FirstAction
  | Here
  | HorrorOnThis ValueMatcher
  | DamageOnThis ValueMatcher
  | InThreatAreaOf InvestigatorMatcher
  | InVictoryDisplay CardMatcher ValueMatcher
  | InvestigatorExists InvestigatorMatcher
  | InvestigatorIsAlone
  | InvestigatorsHaveSpendableClues ValueMatcher
  | LocationExists LocationMatcher
  | AllUndefeatedInvestigatorsResigned
  | EachUndefeatedInvestigator InvestigatorMatcher
  | OnLocation LocationMatcher
  | OnSameLocation
  | OwnCardWithDoom
  | ControlsThis -- really controls this
  | PlayableCardExists ExtendedCardMatcher
  | PlayableCardExistsWithCostReduction Int ExtendedCardMatcher
  | ResourcesOnThis ValueMatcher
  | ResourcesOnLocation Where ValueMatcher
  | ReturnableCardInDiscard DiscardSignifier [Trait]
  | PlayableCardInDiscard DiscardSignifier CardMatcher
  | ScenarioCardHasResignAbility
  | ScenarioDeckWithCard ScenarioDeckKey
  | Self
  | SetAsideCardExists CardMatcher
  | TreacheryExists TreacheryMatcher
  | Unowned
  | SelfHasModifier ModifierType
  | ValueIs Int ValueMatcher
  | UnderneathCardCount ValueMatcher UnderZone CardMatcher
  | Remembered ScenarioLogKeyListMatcher [ScenarioLogKey]
  | ActionCanBeUndone
  | DuringPhase PhaseMatcher
  -- Special Criterion
  | Criteria [Criterion]
  | AnyCriterion [Criterion]
  | NoRestriction
  | Never
  | Negate Criterion
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup EnemyCriterion where
  EnemyMatchesCriteria xs <> EnemyMatchesCriteria ys =
    EnemyMatchesCriteria $ xs <> ys
  EnemyMatchesCriteria xs <> x = EnemyMatchesCriteria $ x : xs
  x <> EnemyMatchesCriteria xs = EnemyMatchesCriteria $ x : xs
  x <> y = EnemyMatchesCriteria [x, y]

data UnderZone = UnderActDeck | UnderAgendaDeck | UnderZones [UnderZone]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup UnderZone where
  UnderZones xs <> UnderZones ys = UnderZones $ xs <> ys
  UnderZones xs <> y = UnderZones $ xs <> [y]
  x <> UnderZones ys = UnderZones $ x : ys
  x <> y = UnderZones [x, y]
