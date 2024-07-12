{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Game (
  module Arkham.Game,
  module X,
) where

import Arkham.Ability.Types
import Arkham.Act.Types
import Arkham.Agenda.Types
import Arkham.Asset.Types
import Arkham.Campaign.Types
import Arkham.Classes.Entity
import Arkham.Classes.GameLogger
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasChaosTokenValue
import Arkham.Classes.HasDistance
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Effect.Types
import Arkham.Enemy.Types
import Arkham.Event.Types
import Arkham.Game.Base as X
import Arkham.Id
import Arkham.Investigator.Types
import Arkham.Location.Types
import Arkham.Matcher
import {-# SOURCE #-} Arkham.Message
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types
import Arkham.Skill.Types
import Arkham.Story.Types
import Arkham.Treachery.Types
import Control.Monad.Random

class HasGameRef a where
  gameRefL :: Lens' a (IORef Game)

class HasStdGen a where
  genL :: Lens' a (IORef StdGen)

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
instance Query StoryMatcher
instance Query ChaosTokenMatcher
instance Query TargetMatcher
instance Query TreacheryMatcher

instance Projection Act
instance Projection Agenda
instance Projection Asset
instance Projection (DiscardedEntity Asset)
instance Projection (InHandEntity Asset)
instance Projection (InDiscardEntity Asset)
instance Projection Campaign
instance Projection Effect
instance Projection Enemy
instance Projection (OutOfPlayEntity Enemy)
instance Projection Event
instance Projection (InHandEntity Event)
instance Projection (InHandEntity Skill)
instance Projection (InDiscardEntity Skill)
instance Projection Investigator
instance Projection Location
instance Projection Scenario
instance Projection Skill
instance Projection Treachery
instance Projection Story

instance HasChaosTokenValue InvestigatorId
instance HasChaosTokenValue ()

delve :: Game -> Game
withoutCanModifiers :: Game -> Game
abilityMatches :: HasGame m => Ability -> AbilityMatcher -> m Bool
asIfTurn :: HasGame m => InvestigatorId -> (forall n. HasGame n => n a) -> m a

instance HasDistance Game
instance HasAbilities Game

class Monad m => HasDebugLevel m where
  getDebugLevel :: m Int

runMessages
  :: ( HasGameRef env
     , HasStdGen env
     , HasQueue Message m
     , MonadReader env m
     , HasGameLogger m
     , HasDebugLevel m
     )
  => Maybe (Message -> IO ())
  -> m ()
