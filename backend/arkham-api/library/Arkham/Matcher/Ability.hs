{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.Ability where

import Arkham.Action
import Arkham.Id
import Arkham.Matcher.Asset
import Arkham.Matcher.Card
import Arkham.Matcher.Enemy
import Arkham.Matcher.Investigator
import Arkham.Matcher.Location
import Arkham.Matcher.Story
import Arkham.Matcher.Window
import {-# SOURCE #-} Arkham.Modifier
import Arkham.Prelude
import {-# SOURCE #-} Arkham.Source
import Data.Aeson.TH
import GHC.OverloadedLabels

data AbilityMatcher
  = AbilityOnLocation LocationMatcher
  | AbilityOnAsset AssetMatcher
  | AbilityOnEnemy EnemyMatcher
  | AbilityOnStory StoryMatcher
  | AbilityWindow WindowMatcher
  | AbilityIsAction Action
  | AbilityIsActionAbility
  | AbilityIsReactionAbility
  | AbilityIsFastAbility
  | AbilityIsForcedAbility
  | AbilityMatches [AbilityMatcher]
  | AbilityOneOf [AbilityMatcher]
  | AbilityIs Source Int
  | AnyAbility
  | BasicAbility
  | ActiveAbility
  | AbilityIsSkillTest
  | AbilityOnEncounterCard
  | AbilityOnCard CardMatcher
  | AbilityOnCardControlledBy InvestigatorId
  | AssetAbility AssetMatcher
  | HauntedAbility
  | PerformableAbility [ModifierType]
  | PerformableAbilityBy InvestigatorMatcher [ModifierType]
  | TriggeredAbility
  | NotAbility AbilityMatcher
  deriving stock (Show, Eq, Ord, Data)

instance IsLabel "parley" AbilityMatcher where
  fromLabel = AbilityIsAction #parley

instance IsLabel "draw" AbilityMatcher where
  fromLabel = AbilityIsAction #draw

instance IsLabel "play" AbilityMatcher where
  fromLabel = AbilityIsAction #play

instance IsLabel "engage" AbilityMatcher where
  fromLabel = AbilityIsAction #engage

instance IsLabel "resource" AbilityMatcher where
  fromLabel = AbilityIsAction #resource

instance IsLabel "move" AbilityMatcher where
  fromLabel = AbilityIsAction #move

instance IsLabel "resign" AbilityMatcher where
  fromLabel = AbilityIsAction #resign

instance IsLabel "investigate" AbilityMatcher where
  fromLabel = AbilityIsAction #investigate

instance IsLabel "evade" AbilityMatcher where
  fromLabel = AbilityIsAction #evade

instance IsLabel "fight" AbilityMatcher where
  fromLabel = AbilityIsAction #fight

instance IsLabel "action" AbilityMatcher where
  fromLabel = AbilityIsActionAbility

instance IsLabel "basic" AbilityMatcher where
  fromLabel = BasicAbility

instance Not AbilityMatcher where
  not_ = NotAbility

instance Semigroup AbilityMatcher where
  AnyAbility <> x = x
  x <> AnyAbility = x
  AbilityMatches xs <> AbilityMatches ys = AbilityMatches $ xs <> ys
  AbilityMatches xs <> x = AbilityMatches $ xs <> [x]
  x <> AbilityMatches xs = AbilityMatches $ x : xs
  x <> y = AbilityMatches [x, y]

instance Monoid AbilityMatcher where
  mempty = AnyAbility

$(deriveJSON defaultOptions ''AbilityMatcher)
