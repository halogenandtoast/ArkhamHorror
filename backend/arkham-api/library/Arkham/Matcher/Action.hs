{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.Action where

import Arkham.Action
import Arkham.Prelude
import Data.Aeson.TH
import GHC.OverloadedLabels

data ActionMatcher
  = ActionIs Action
  | AnyAction
  | ActionOneOf [ActionMatcher]
  | ActionMatches [ActionMatcher]
  | RepeatableAction
  deriving stock (Show, Eq, Ord, Data)

instance Semigroup ActionMatcher where
  AnyAction <> x = x
  x <> AnyAction = x
  ActionMatches xs <> ActionMatches ys = ActionMatches $ xs <> ys
  ActionMatches xs <> x = ActionMatches $ xs <> [x]
  x <> ActionMatches xs = ActionMatches $ x : xs
  x <> y = ActionMatches [x, y]

instance Monoid ActionMatcher where
  mempty = AnyAction

instance IsLabel "activate" ActionMatcher where
  fromLabel = ActionIs #activate

instance IsLabel "explore" ActionMatcher where
  fromLabel = ActionIs #explore

instance IsLabel "engage" ActionMatcher where
  fromLabel = ActionIs #engage

instance IsLabel "evade" ActionMatcher where
  fromLabel = ActionIs #evade

instance IsLabel "fight" ActionMatcher where
  fromLabel = ActionIs #fight

instance IsLabel "investigate" ActionMatcher where
  fromLabel = ActionIs #investigate

instance IsLabel "move" ActionMatcher where
  fromLabel = ActionIs #move

instance IsLabel "parley" ActionMatcher where
  fromLabel = ActionIs #parley

instance IsLabel "play" ActionMatcher where
  fromLabel = ActionIs #play

instance IsLabel "resource" ActionMatcher where
  fromLabel = ActionIs #resource

instance IsLabel "draw" ActionMatcher where
  fromLabel = ActionIs #draw

$(deriveJSON defaultOptions ''ActionMatcher)
