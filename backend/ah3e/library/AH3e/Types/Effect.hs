module AH3e.Types.Effect where

import AH3e.Prelude
import AH3e.Types.Ids
import AH3e.Types.Skill

data AssetDeckKind = ItemDeckKind | AllyDeckKind | SpellDeckKind
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | What a card charges for something bought from a deck or the display.
data Pricing = FullPrice | HalfPrice | FlatPrice Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ValueBound = AtMost Int | AtLeast Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Count
  = CluesYouHave
  | ItemsYouHave
  | SpellsYouHave
  | DoomInYourSpace
  | MonstersInYourNeighborhood
  | CluesInYourNeighborhood
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Amount = N Int | TestResult | Half Amount | Counted Count | Diff Amount Amount
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype ConditionName = ConditionName Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, IsString)

{- | Who a 'ForInvestigators' effect fans out over. Source-relative scopes read
the effect's source, so a lurking monster measures from its own space.
-}
data InvestigatorScope = EveryInvestigator | NearestToSource | InSourceNeighborhood
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Recipient
  = You
  | YouOrAlly
  | InvestigatorInYourSpace
  | InvestigatorOrAllyInYourSpace
  | EachInvestigatorInYourSpace
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Where
  = YourSpace
  | SpaceInYourNeighborhood
  | OtherSpaceInYourNeighborhood
  | AnySpace
  | DifferentSpaces Int [SpaceId]
  | EachSpaceInYourNeighborhood
  | TheSpace SpaceId
  | TheUnstableSpace
  | AdjacentStreet
  | AdjacentSpace
  | YourSpaceOrAdjacent
  | AnySpaceWithDoom
  | AdjacentSpaceWithMostDoom
  | SourceSpace
  | ScenarioSheet
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CardFilter = AnyCard | WithTrait Trait | NamedCard Text | ItemCard | AllyCard | SpellCard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Cost
  = SpendMoney Int
  | SpendRemnants Int
  | SpendClues Int
  | SpendFocus Int
  | CostDamage Int
  | CostHorror Int
  | CostDelayed
  | CostCondition ConditionName
  | CostDiscard CardFilter
  | AllOf [Cost]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Predicate
  = HasMoney Int
  | HasClues Int
  | HasRemnants Int
  | HasCondition ConditionName
  | HasCard CardFilter
  | IsDelayed
  | CodexHas ArchiveNumber
  | Not Predicate
  | CustomPredicate Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Gain
  = Money Amount
  | Clues Amount
  | ClueFromNeighborhood
  | Remnants Amount
  | AnItem (Maybe Trait)
  | AnItemValued (Maybe Trait) ValueBound
  | AnAlly (Maybe Trait)
  | ASpell (Maybe Trait)
  | Named Text
  | Condition ConditionName
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Effect
  = Seq [Effect]
  | -- | Resolve the effect once per investigator in scope, as that investigator.
    ForInvestigators InvestigatorScope Effect
  | NoEffect
  | Test Skill Int Effect Effect
  | ByResult [((Int, Maybe Int), Effect)]
  | ForEachOf Count Effect
  | MayPay Cost Effect Effect
  | RepeatWhilePaying Cost Effect
  | Pay Cost Effect
  | May Text Effect
  | Choose [(Text, Effect)]
  | If Predicate Effect Effect
  | GainE Gain
  | LoseMoney Amount
  | BuyFromDisplay (Maybe Trait) Bool (Maybe Int) Effect
  | Focus (Maybe Skill) Bool
  | DiscardAFocus
  | BuyFromDeck AssetDeckKind Int (Maybe Int) Pricing
  | PlaceCluesOnSheet Amount
  | DoomOnSheet Amount
  | SufferDamage Amount
  | SufferHorror Amount
  | SufferHarmE Amount Amount
  | DirectDamage Amount
  | DirectHorror Amount
  | RecoverHealth Recipient Amount
  | RecoverSanity Recipient Amount
  | RecoverBoth Recipient Amount Amount
  | RemoveDoomFrom Where Amount
  | PlaceDoomAt Where Amount
  | SpreadDoomOnce
  | SpawnOneClue
  | SpawnMonster
  | SpawnMonsterIn Where Bool
  | -- | one monster of their choice in reach suffers this much damage
    DamageMonsterIn Where Amount
  | ResolveGateBurst
  | ReadHeadline
  | DrawMythosTokens Int
  | BecomeDelayed
  | BecomeDevoured
  | Retire
  | MoveUpTo Int
  | MoveUpToIgnoringMonsters Int
  | MoveDirectlyTo Where
  | AddToCodex ArchiveNumber
  | FlipArchiveCard ArchiveNumber
  | RemoveFromCodex ArchiveNumber
  | WinGame
  | LoseGame
  | Custom Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
