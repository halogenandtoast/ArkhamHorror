module AH3e.Types.Card where

import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill

data Expansion = CoreSet | DeadOfNight | UnderDarkWaves | SecretsOfTheOrder | RecursiveEchoes
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Encounter = Encounter {text :: Text, effect :: Effect}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data AssetType = Ally | Item | Spell | Talent | ConditionAsset
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

data AssetOrigin = AllyDeck | ItemDeck | SpellDeck | SpecialPile | StartingPile | ConditionPile | Archive
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

data AssetDef = AssetDef
  { assetType :: AssetType
  , origin :: AssetOrigin
  , traits :: [Trait]
  , value :: Maybe Int
  , hands :: Int
  , health :: Maybe Int
  , sanity :: Maybe Int
  , spellHorror :: Int
  , text :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ConditionFace = ConditionFace {name :: ConditionName, text :: Text}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ConditionDef = ConditionDef
  { front :: ConditionFace
  , back :: ConditionFace
  , backIsCondition :: Bool
  , hiddenBack :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Keyword = Elusive | Feed | Massive | Watcher | Pursuit | Relentless | Retaliate | Shrouded
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

data InvestigatorRule
  = LowestSkill Skill
  | HighestSkill Skill
  | MostClues
  | FewestClues
  | MostMoney
  | MostRemnants
  | MostSpells
  | MostAllies
  | MostDamage
  | LeastDamage
  | MostItems
  | -- | Every investigator; the hunter pathway already prefers the closest.
    NearestInvestigator
  | LowestRemainingHealth
  | LowestRemainingSanity
  | TheLeader
  | CustomInvestigatorRule Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SpaceRule
  = UnstableSpace
  | MostDoomSpace
  | StartingSpace
  | NamedSpace SpaceId
  | PreySpace InvestigatorRule
  | -- | A street nearest the given investigators; 'Nothing' means the monster's own prey.
    NearestStreetTo (Maybe InvestigatorRule)
  | CustomSpaceRule Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Activation
  = Hunter InvestigatorRule
  | Patrol SpaceRule (Maybe InvestigatorRule)
  | Lurker Effect
  | CustomActivation Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data MonsterDef = MonsterDef
  { readyName :: Maybe Text
  , spawn :: SpaceRule
  , activation :: Activation
  , speed :: Int
  , traits :: [Trait]
  , health :: Int
  , elite :: Int
  , attackSkill :: Skill
  , attackModifier :: Int
  , evadeModifier :: Int
  , damage :: Int
  , horror :: Int
  , remnant :: Bool
  , keywords :: [Keyword]
  , epic :: Bool
  , text :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EventDef = EventDef
  { scenario :: ScenarioCode
  , neighborhood :: NeighborhoodId
  , encounters :: Map SpaceId Encounter
  , doomSpaces :: [SpaceId]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data HeadlineDef = HeadlineDef {rumor :: Bool, text :: Text, effect :: Effect, reckoning :: Maybe Effect}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data AnomalyDef = AnomalyDef {set :: Text, byDoom :: [((Int, Maybe Int), Encounter)]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ArchiveSide = ArchiveSide {text :: Text}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ArchiveDef = ArchiveDef {number :: ArchiveNumber, front :: ArchiveSide, back :: Maybe ArchiveSide}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TerrorDef = TerrorDef {set :: Text, byTerror :: [((Int, Maybe Int), Encounter)]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data MysteryDef = MysteryDef {space :: SpaceId, opening :: Encounter, branches :: [(Text, Encounter)]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CardKind
  = MonsterCard MonsterDef
  | AssetCard AssetDef
  | ConditionCard ConditionDef
  | NeighborhoodCard NeighborhoodId (Map SpaceId Encounter)
  | StreetCard (Map StreetType Encounter)
  | EventCard EventDef
  | HeadlineCard HeadlineDef
  | AnomalyCard AnomalyDef
  | ArchiveCard ArchiveDef
  | TerrorCard TerrorDef
  | MysteryCard MysteryDef
  | TravelRouteCard (Map RouteType Encounter)
  | ThresholdCard (Map ThresholdType Encounter)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CardDef = CardDef
  { code :: CardCode
  , name :: Text
  , expansion :: Expansion
  , copies :: Int
  , kind :: CardKind
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data StartingPossession
  = StartingCard CardCode
  | StartingMoney Int
  | StartingRemnants Int
  | StartingClues Int
  | StartingCondition ConditionName
  | StartingChoice [[StartingPossession]]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Role = Guardian | Mystic | Rogue | Seeker | Survivor
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

data InvestigatorDef = InvestigatorDef
  { id :: InvestigatorId
  , name :: Text
  , occupation :: Text
  , expansion :: Expansion
  , health :: Int
  , sanity :: Int
  , focusLimit :: Maybe Int
  , skills :: Map Skill Int
  , starting :: [StartingPossession]
  , roles :: [Role]
  , abilityText :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data MythosToken
  = SpreadDoomToken
  | SpawnMonsterToken
  | ReadHeadlineToken
  | SpawnClueToken
  | GateBurstToken
  | ReckoningToken
  | BlankToken
  | SpreadTerrorToken
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SpaceDef = SpaceDef {id :: SpaceId, name :: Text, kind :: SpaceKind, neighborhood :: Maybe NeighborhoodId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data NeighborhoodDef = NeighborhoodDef {id :: NeighborhoodId, name :: Text, town :: Town, spaces :: [SpaceDef]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data MapDef = MapDef
  { neighborhoods :: [NeighborhoodDef]
  , otherSpaces :: [SpaceDef]
  , borders :: [(SpaceId, SpaceId, Maybe Hazard)]
  , layout :: BoardLayout
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ScenarioDef = ScenarioDef
  { code :: ScenarioCode
  , name :: Text
  , expansion :: Expansion
  , startingSpace :: SpaceId
  , reckoningText :: Text
  , reckoning :: Effect
  , setupMap :: MapDef
  , monsters :: [(CardCode, Int)]
  , startingMonsters :: [(CardCode, SpaceId)]
  , mythosCup :: [(MythosToken, Int)]
  , startingDoom :: [SpaceId]
  , eventCards :: [CardCode]
  , codex :: [ArchiveNumber]
  , anomalySet :: Maybe Text
  , terrorSet :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
