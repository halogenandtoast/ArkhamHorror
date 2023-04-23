{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Game
  ( module Arkham.Game
  , module X
  ) where

import Arkham.Ability.Types
import Arkham.Act.Types
import Arkham.Agenda.Types
import Arkham.Asset.Types
import Arkham.Card
import Arkham.Campaign.Types
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasDistance
import Arkham.Classes.HasTokenValue
import Arkham.Classes.Query
import Arkham.Effect.Types
import Arkham.Enemy.Types
import Arkham.Event.Types
import Arkham.Game.Base as X
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Investigator.Types
import Arkham.Location.Types
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types
import Arkham.Skill.Types
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
instance Query TokenMatcher
instance Query TreacheryMatcher

instance Projection Act
instance Projection Agenda
instance Projection Asset
instance Projection (DiscardedEntity Asset)
instance Projection (InHandEntity Asset)
instance Projection Campaign
instance Projection Effect
instance Projection Enemy
instance Projection (OutOfPlayEntity Enemy)
instance Projection Event
instance Projection (InHandEntity Event)
instance Projection Investigator
instance Projection Location
instance Projection Scenario
instance Projection Skill
instance Projection Treachery

instance HasTokenValue InvestigatorId
instance HasTokenValue ()

delve :: Game -> Game
withoutCanModifiers :: Game -> Game
withCardEntity :: InvestigatorId -> Card -> Game -> Game
abilityMatches :: HasGame m => Ability -> AbilityMatcher -> m Bool

instance HasDistance Game
instance HasAbilities Game
