{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Game where

import Arkham.Query
import Arkham.Ability
import Arkham.Act.Types
import Arkham.Agenda.Types
import Arkham.Asset.Types
import Arkham.Campaign.Types
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.HasTokenValue
import Arkham.Classes.HasDistance
import Arkham.Classes.HasAbilities
import Arkham.Classes.Query
import Arkham.Effect.Types
import Arkham.Enemy.Types
import Arkham.Event.Types
import Arkham.History
import Arkham.Id
import Arkham.Investigator.Types
import Arkham.Location.Types
import Arkham.Matcher
import Arkham.Phase
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types
import Arkham.Skill.Types
import Arkham.SkillTest.Base
import Arkham.Treachery.Types
import Control.Monad.Random

data Game

class HasGameRef a where
  gameRefL :: Lens' a (IORef Game)

class HasStdGen a where
  genL :: Lens' a (IORef StdGen)

instance HasModifiersFor ()

instance Query AbilityMatcher
instance Query ActMatcher
instance Query AgendaMatcher
instance Query AssetMatcher
instance Query CardMatcher
instance Query CampaignMatcher
instance Query EffectMatcher
instance Query EnemyMatcher
instance Query EventMatcher
instance Query ExtendedCardMatcher
instance Query InvestigatorMatcher
instance Query LocationMatcher
instance Query PreyMatcher
instance Query RemainingActMatcher
instance Query ScenarioMatcher
instance Query SkillMatcher
instance Query TreacheryMatcher

instance Projection Act
instance Projection Agenda
instance Projection Asset
instance Projection (DiscardedEntity Asset)
instance Projection Campaign
instance Projection Effect
instance Projection Enemy
instance Projection Event
instance Projection Investigator
instance Projection Location
instance Projection Scenario
instance Projection Skill
instance Projection Treachery

instance (FieldDict Eq Asset, FieldDict Typeable Asset, FieldDict Typeable Location, FieldDict Eq Location) => Queryable Asset
instance (FieldDict Eq Enemy, FieldDict Typeable Enemy, FieldDict Typeable Location, FieldDict Eq Location) => Queryable Enemy
instance (FieldDict Eq Location, FieldDict Typeable Location) => Queryable Location

instance HasTokenValue InvestigatorId
instance HasTokenValue ()

gameActiveAbilities :: Game -> [Ability]
gamePhase :: Game -> Phase
gameSkillTest :: Game -> Maybe SkillTest
gameWindowDepth :: Game -> Int
gameDepthLock :: Game -> Int
gamePhaseHistory :: Game -> HashMap InvestigatorId History
gameTurnHistory :: Game -> HashMap InvestigatorId History
gameRoundHistory :: Game -> HashMap InvestigatorId History
delve :: Game -> Game

-- Time Warp
gameActionCanBeUndone :: Game -> Bool

instance HasDistance Game
instance HasAbilities Game
