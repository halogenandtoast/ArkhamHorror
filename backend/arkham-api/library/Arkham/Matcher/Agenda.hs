{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.Agenda where

import Arkham.Agenda.Sequence
import Arkham.Id
import Arkham.Matcher.Act
import {-# SOURCE #-} Arkham.Matcher.Asset
import Arkham.Matcher.Base
import Arkham.Matcher.Enemy
import {-# SOURCE #-} Arkham.Matcher.Event
import {-# SOURCE #-} Arkham.Matcher.Investigator
import Arkham.Matcher.Location
import Arkham.Matcher.Skill
import Arkham.Matcher.Treachery
import Arkham.Matcher.Value
import {-# SOURCE #-} Arkham.Modifier
import Arkham.Prelude
import Data.Aeson.TH

data AgendaMatcher
  = AgendaWithId AgendaId
  | AgendaWithDoom ValueMatcher
  | AnyAgenda
  | AgendaWithTreachery TreacheryMatcher
  | AgendaWithSequence AgendaSequence
  | AgendaWithSide AgendaSide
  | AgendaWithDeckId Int
  | AgendaWithModifier ModifierType
  | NotAgenda AgendaMatcher
  | AgendaMatches [AgendaMatcher]
  | AgendaCanWheelOfFortuneX
  | FinalAgenda
  deriving stock (Show, Eq, Ord, Data)

instance IsMatcher AgendaMatcher

instance Semigroup AgendaMatcher where
  AnyAgenda <> x = x
  x <> AnyAgenda = x
  AgendaMatches xs <> AgendaMatches ys = AgendaMatches (xs <> ys)
  AgendaMatches xs <> x = AgendaMatches (x : xs)
  x <> AgendaMatches xs = AgendaMatches (x : xs)
  x <> y = AgendaMatches [x, y]

data RemoveDoomMatchers = RemoveDoomMatchers
  { removeDoomLocations :: LocationMatcher
  , removeDoomInvestigators :: InvestigatorMatcher
  , removeDoomEnemies :: EnemyMatcher
  , removeDoomAssets :: AssetMatcher
  , removeDoomActs :: ActMatcher
  , removeDoomAgendas :: AgendaMatcher
  , removeDoomTreacheries :: TreacheryMatcher
  , removeDoomEvents :: EventMatcher
  , removeDoomSkills :: SkillMatcher
  }
  deriving stock (Show, Eq, Ord, Data)

mconcat
  [ deriveJSON defaultOptions ''AgendaMatcher
  , deriveJSON defaultOptions ''RemoveDoomMatchers
  ]
