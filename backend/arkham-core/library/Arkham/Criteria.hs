{-# LANGUAGE TemplateHaskell #-}

module Arkham.Criteria (
  module Arkham.Criteria,
  module Arkham.Criteria.Override,
) where

import Arkham.Prelude

import Arkham.CampaignLogKey (CampaignLogKey)
import Arkham.Campaigns.TheForgottenAge.Supply (Supply)
import Arkham.Capability (Capabilities, Capable (..))
import Arkham.Cost.Status (CostStatus)
import Arkham.Criteria.Override
import Arkham.Direction (GridDirection)
import Arkham.GameValue (GameValue (Static))
import Arkham.History.Types (HistoryType)
import Arkham.Matcher
import Arkham.Modifier
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
pattern CanGainResources <-
  InvestigatorExists (InvestigatorMatches [You, InvestigatorWithoutModifier CannotGainResources])
  where
    CanGainResources = InvestigatorExists (InvestigatorMatches [You, InvestigatorWithoutModifier CannotGainResources])

pattern CanDealDamage :: Criterion
pattern CanDealDamage <-
  InvestigatorExists (InvestigatorMatches [You, InvestigatorWithoutModifier CannotDealDamage])
  where
    CanDealDamage = InvestigatorExists (InvestigatorMatches [You, InvestigatorWithoutModifier CannotDealDamage])

pattern CanAttack :: Criterion
pattern CanAttack <-
  InvestigatorExists (InvestigatorMatches [You, InvestigatorWithoutModifier CannotAttack])
  where
    CanAttack = InvestigatorExists (InvestigatorMatches [You, InvestigatorWithoutModifier CannotAttack])

pattern CanDiscoverCluesAt :: LocationMatcher -> Criterion
pattern CanDiscoverCluesAt locationMatcher =
  InvestigatorExists (InvestigatorMatches [You, InvestigatorCanDiscoverCluesAt locationMatcher])

-- TODO: This is too close in name to CanDiscoverCluesAt, need to determine if CanDiscoverCluesAt needs to exist
pattern AbleToDiscoverCluesAt :: LocationMatcher -> Criterion
pattern AbleToDiscoverCluesAt locationMatcher =
  Criteria [OnLocation LocationWithAnyClues, CanDiscoverCluesAt locationMatcher]

pattern CanTakeControlOfClues :: Criterion
pattern CanTakeControlOfClues <-
  InvestigatorExists (InvestigatorMatches [You, InvestigatorWithoutModifier CannotTakeControlOfClues])
  where
    CanTakeControlOfClues =
      InvestigatorExists (InvestigatorMatches [You, InvestigatorWithoutModifier CannotTakeControlOfClues])

pattern CanDrawCards :: Criterion
pattern CanDrawCards <-
  Criteria
    [ CanManipulateDeck
      , InvestigatorExists (InvestigatorMatches [You, InvestigatorWithoutModifier CannotDrawCards])
      ]
  where
    CanDrawCards =
      Criteria
        [ CanManipulateDeck
        , InvestigatorExists (InvestigatorMatches [You, InvestigatorWithoutModifier CannotDrawCards])
        ]

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
  InvestigatorExists (InvestigatorMatches [You, InvestigatorWithoutModifier CannotManipulateDeck])
  where
    CanManipulateDeck = InvestigatorExists (InvestigatorMatches [You, InvestigatorWithoutModifier CannotManipulateDeck])

pattern InYourThreatArea :: Criterion
pattern InYourThreatArea <- InThreatAreaOf You
  where
    InYourThreatArea = InThreatAreaOf You

pattern DuringAnySkillTest :: Criterion
pattern DuringAnySkillTest <- DuringSkillTest AnySkillTest
  where
    DuringAnySkillTest = DuringSkillTest AnySkillTest

data Criterion
  = AssetExists AssetMatcher
  | EventExists EventMatcher
  | ExcludeWindowAssetExists AssetMatcher
  | AgendaExists AgendaMatcher
  | ActExists ActMatcher
  | SkillExists SkillMatcher
  | StoryExists StoryMatcher
  | InYourHand
  | InYourDiscard
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
  | AssetCount Int AssetMatcher
  | EventCount ValueMatcher EventMatcher
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
  | EncounterDeckWith CardListMatcher
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
  | CanMoveTo LocationMatcher
  | NotYetRecorded CampaignLogKey
  | HasHistory HistoryType InvestigatorMatcher HistoryMatcher
  | HasScenarioCount ScenarioCountKey ValueMatcher
  | HasRemainingBlessTokens
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

instance Not Criterion where
  not_ = Negate

instance OneOf Criterion where
  oneOf = AnyCriterion

onLocation :: IsLocationMatcher a => a -> Criterion
onLocation = OnLocation . toLocationMatcher

cluesOnThis :: Int -> Criterion
cluesOnThis = CluesOnThis . atLeast

enemyExists :: EnemyMatcher -> Criterion
enemyExists = EnemyCriteria . EnemyExists

anyInvestigatorExists :: [InvestigatorMatcher] -> Criterion
anyInvestigatorExists = exists . AnyInvestigator

atYourLocation :: InvestigatorMatcher -> Criterion
atYourLocation matcher = exists (AtYourLocation <> matcher)

class Exists a where
  exists :: a -> Criterion

notExists :: Exists a => a -> Criterion
notExists = not_ . exists

instance Exists EventMatcher where
  exists = EventExists

instance Exists StoryMatcher where
  exists = StoryExists

instance Exists SkillMatcher where
  exists = SkillExists

instance Exists AgendaMatcher where
  exists = AgendaExists

instance Exists InvestigatorMatcher where
  exists = InvestigatorExists

you :: InvestigatorMatcher -> Criterion
you a = exists (You <> a)

instance Exists AssetMatcher where
  exists = AssetExists

instance Exists LocationMatcher where
  exists = LocationExists

instance Exists TreacheryMatcher where
  exists = TreacheryExists

instance Exists EnemyMatcher where
  exists = enemyExists

instance Exists ExtendedCardMatcher where
  exists = ExtendedCardExists

youExist :: InvestigatorMatcher -> Criterion
youExist matcher = exists (You <> matcher)

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

instance Capable (InvestigatorMatcher -> Maybe Criterion) where
  can =
    let can' = can :: Capabilities (InvestigatorMatcher -> Criterion)
     in fmap (fmap Just) can'

instance Capable (InvestigatorMatcher -> Criterion) where
  can =
    let can' = can :: Capabilities InvestigatorMatcher
     in fmap (\m matcher -> exists (m <> matcher)) can'

$(deriveJSON defaultOptions ''DiscardSignifier)
$(deriveJSON defaultOptions ''UnderZone)
$(deriveJSON defaultOptions ''EnemyCriterion)
$(deriveJSON defaultOptions ''Criterion)
