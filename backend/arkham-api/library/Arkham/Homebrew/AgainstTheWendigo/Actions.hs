{-# LANGUAGE TemplateHaskell #-}

{- | Actions owned by Against the Wendigo.

The valley is crossed by water: a @River@ location is only reachable from
another @River@ location by one of these two actions (see the scenario's
additional rules), so both are gated on standing on one.

* @WalkAlongTheRiver@ costs two actions and moves to a connected @River@
  location.
* @Navigate@ costs one action, once per round, and resolves in four steps:
  attacks of opportunity, disengage, move up to three @River@ locations, then a
  [combat]/[agility] test whose difficulty scales with the distance moved. Guide
  assets boost that fourth step, and Rapid raises it.
-}
module Arkham.Homebrew.AgainstTheWendigo.Actions (module Arkham.Homebrew.AgainstTheWendigo.Actions) where

import Arkham.Action (Action)
import Arkham.Criteria (Criterion (OnLocation))
import Arkham.Homebrew.TH (declareHomebrewActions)
import Arkham.Matcher (LocationMatcher (LocationWithTrait))
import Arkham.Trait (Trait (River))

declareHomebrewActions
  [ "Navigate"
  , "WalkAlongTheRiver"
  ]

-- | Both river actions are only ever affordable from a River location.
actionAffordability :: [(Action, Criterion)]
actionAffordability =
  [ (Navigate, OnLocation (LocationWithTrait River))
  , (WalkAlongTheRiver, OnLocation (LocationWithTrait River))
  ]
