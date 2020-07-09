module Arkham.Types.GameState
  ( ArkhamGameState(..)
  , ArkhamGameStateLock(..)
  , ArkhamGameStateStep(..)
  , ArkhamStack(..)
  , ArkhamPhase(..)
  , ArkhamAct(..)
  , ArkhamAgenda(..)
  , ArkhamSkillCheckStep(..)
  , ArkhamRevealTokenStep(..)
  , ArkhamResolveEnemiesStep(..)
  , ArkhamAttackOfOpportunityStep(..)
  , ArkhamChooseOneStep(..)
  , ArkhamTarget(..)
  , stackAgenda
  , stackAct
  , _ActStack
  , _AgendaStack
  , _TopOfStack
  )
where

import Arkham.Types.Act
import Arkham.Types.Action
import Arkham.Types.Agenda
import Arkham.Types.Card
import Arkham.Types.ChaosToken
import Arkham.Types.Enemy
import Arkham.Types.Location
import Arkham.Types.Player
import Arkham.Types.Skill
import Base.Lock
import ClassyPrelude
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.UUID
import Entity.User
import Json
import Lens.Micro
import Orphans ()

data ArkhamPhase = Mythos | Investigation | Enemy | Upkeep
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

data ArkhamGameStateStep
  = ArkhamGameStateStepInvestigatorActionStep
  | ArkhamGameStateStepSkillCheckStep ArkhamSkillCheckStep
  | ArkhamGameStateStepRevealTokenStep ArkhamRevealTokenStep
  | ArkhamGameStateStepResolveEnemiesStep ArkhamResolveEnemiesStep
  | ArkhamGameStateStepAttackOfOpportunityStep ArkhamAttackOfOpportunityStep
  | ArkhamGameStateStepChooseOneStep ArkhamChooseOneStep
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

instance Eq ArkhamGameStateStep where
  ArkhamGameStateStepInvestigatorActionStep == ArkhamGameStateStepInvestigatorActionStep
    = True
  ArkhamGameStateStepSkillCheckStep _ == ArkhamGameStateStepSkillCheckStep _
    = True
  ArkhamGameStateStepRevealTokenStep _ == ArkhamGameStateStepRevealTokenStep _
    = True
  _ == _ = False

data ArkhamTarget = LocationTarget ArkhamLocation | EnemyTarget UUID | AgendaTarget ArkhamAgenda
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

data ArkhamAttackOfOpportunityStep = ArkhamAttackOfOpportunityStep
  { aoosEnemyIds :: HashSet UUID
  , aoosPlayerId :: UUID
  , aoosNextStateStep :: ArkhamGameStateStep
  }
  deriving stock (Generic, Show)

instance ToJSON ArkhamAttackOfOpportunityStep where
  toJSON = genericToJSON $ aesonOptions $ Just "aoos"
  toEncoding = genericToEncoding $ aesonOptions $ Just "aoos"

instance FromJSON ArkhamAttackOfOpportunityStep where
  parseJSON = genericParseJSON $ aesonOptions $ Just "aoos"

data ArkhamChooseOneStep = ArkhamChooseOneStep
  { acosPlayerId :: UUID
  , acosChoices :: [String]
  , acosChoiceTarget :: ArkhamTarget
  }
  deriving stock (Generic, Show)

instance ToJSON ArkhamChooseOneStep where
  toJSON = genericToJSON $ aesonOptions $ Just "acos"
  toEncoding = genericToEncoding $ aesonOptions $ Just "acos"

instance FromJSON ArkhamChooseOneStep where
  parseJSON = genericParseJSON $ aesonOptions $ Just "acos"

data ArkhamSkillCheckStep = ArkhamSkillCheckStep
  { ascsType :: ArkhamSkillType
  , ascsAction :: ArkhamAction
  }
  deriving stock (Generic, Show)

instance ToJSON ArkhamSkillCheckStep where
  toJSON = genericToJSON $ aesonOptions $ Just "ascs"
  toEncoding = genericToEncoding $ aesonOptions $ Just "ascs"

instance FromJSON ArkhamSkillCheckStep where
  parseJSON = genericParseJSON $ aesonOptions $ Just "ascs"

data ArkhamRevealTokenStep = ArkhamRevealTokenStep
  { artsType :: ArkhamSkillType
  , artsAction :: ArkhamAction
  , artsToken :: ArkhamChaosToken
  , artsDifficulty :: Int
  , artsModifiedSkillValue :: Int
  , artsCards :: [ArkhamCard]
  }
  deriving stock (Generic, Show)

instance ToJSON ArkhamRevealTokenStep where
  toJSON = genericToJSON $ aesonOptions $ Just "arts"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arts"

instance FromJSON ArkhamRevealTokenStep where
  parseJSON = genericParseJSON $ aesonOptions $ Just "arts"

newtype ArkhamResolveEnemiesStep = ArkhamResolveEnemiesStep
  { aresEnemyIds :: HashSet UUID }
  deriving stock (Generic, Show)

instance ToJSON ArkhamResolveEnemiesStep where
  toJSON = genericToJSON $ aesonOptions $ Just "ares"
  toEncoding = genericToEncoding $ aesonOptions $ Just "ares"

instance FromJSON ArkhamResolveEnemiesStep where
  parseJSON = genericParseJSON $ aesonOptions $ Just "ares"

data ArkhamGameStateLock
  = AddDoom
  | DrawEncounter
  | InvestigationTakeActions
  | UpkeepResetActions
  | DrawAndGainResources
  | ResolveEnemies
  | ResolveAttacksOfOpportunity
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ArkhamGameState = ArkhamGameState
  { agsUsers :: HashMap UserId UUID
  , agsPlayers :: HashMap UUID ArkhamPlayer
  , agsPhase :: ArkhamPhase
  , agsChaosBag :: NonEmpty ArkhamChaosToken
  , agsLocations :: HashMap ArkhamCardCode ArkhamLocation
  , agsEnemies :: HashMap UUID ArkhamEnemy
  , agsStacks :: HashMap Text ArkhamStack
  , agsEncounterDeck :: [ArkhamEncounterCard]
  , agsEncounterDiscard :: [ArkhamEncounterCard]
  , agsStep :: ArkhamGameStateStep
  , agsLock :: Maybe (NonEmpty ArkhamGameStateLock)
  , agsActiveUser :: UserId
  }
  deriving stock (Generic, Show)

instance HasLock ArkhamGameState where
  type Lock ArkhamGameState = NonEmpty ArkhamGameStateLock
  lock = lens agsLock $ \m x -> m { agsLock = x }

instance Unlock (NonEmpty ArkhamGameStateLock) where
  type Key (NonEmpty ArkhamGameStateLock) = ArkhamGameStateLock
  unlocks = (==) . NE.head

data ArkhamStack = ActStack (NonEmpty ArkhamAct) | AgendaStack (NonEmpty ArkhamAgenda)
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

stackAct :: ArkhamStack -> Maybe ArkhamAct
stackAct (ActStack acts) = Just $ NE.head acts
stackAct _ = Nothing

stackAgenda :: ArkhamStack -> Maybe ArkhamAgenda
stackAgenda (AgendaStack agendas) = Just $ NE.head agendas
stackAgenda _ = Nothing

_ActStack :: Traversal' ArkhamStack (NonEmpty ArkhamAct)
_ActStack f (ActStack a) = ActStack <$> f a
_ActStack _ (AgendaStack a) = pure $ AgendaStack a

_AgendaStack :: Traversal' ArkhamStack (NonEmpty ArkhamAgenda)
_AgendaStack f (AgendaStack a) = AgendaStack <$> f a
_AgendaStack _ (ActStack a) = pure $ ActStack a

_TopOfStack :: Lens' (NonEmpty a) a
_TopOfStack = lens NE.head $ \(_ :| as) x -> x :| as

instance ToJSON ArkhamGameState where
  toJSON = genericToJSON $ aesonOptions $ Just "ags"
  toEncoding = genericToEncoding $ aesonOptions $ Just "ags"

instance FromJSON ArkhamGameState where
  parseJSON = genericParseJSON $ aesonOptions $ Just "ags"
