{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Matcher.Treachery where

import Arkham.Card.CardCode
import Arkham.Card.Id
import Arkham.Id
import Arkham.Matcher.Base
import Arkham.Matcher.Enemy
import {-# SOURCE #-} Arkham.Matcher.Investigator
import {-# SOURCE #-} Arkham.Matcher.Location
import Arkham.Matcher.Value
import {-# SOURCE #-} Arkham.Modifier
import {-# SOURCE #-} Arkham.Placement
import Arkham.Prelude
import {-# SOURCE #-} Arkham.Target
import Arkham.Token
import Arkham.Trait (Trait)
import Data.Aeson.TH

instance IsMatcher TreacheryMatcher

instance Be TreacheryId TreacheryMatcher where
  be = TreacheryWithId

data TreacheryMatcher
  = TreacheryWithTitle Text
  | TreacheryWithFullTitle Text Text
  | TreacheryWithId TreacheryId
  | TreacheryWithPlacement Placement
  | TreacheryWithToken Token
  | TreacheryWithDoom ValueMatcher
  | TreacheryWithHorror ValueMatcher
  | TreacheryWithTrait Trait
  | TreacheryInHandOf InvestigatorMatcher
  | TreacheryInThreatAreaOf InvestigatorMatcher
  | TreacheryIs CardCode
  | TreacheryIsAttachedTo Target
  | TreacheryAttachedToLocation LocationMatcher
  | TreacheryWithCardId CardId
  | TreacheryAt LocationMatcher
  | TreacheryWithVictory
  | TreacheryOnEnemy EnemyMatcher
  | TreacheryIsNonWeakness
  | TreacheryWithResolvedEffectsBy InvestigatorMatcher
  | TreacheryDiscardedBy InvestigatorMatcher
  | TreacheryWithModifier ModifierType
  | AnyTreachery
  | InPlayTreachery
  | HiddenTreachery
  | TreacheryOwnedBy InvestigatorMatcher
  | TreacheryMatches [TreacheryMatcher]
  | TreacheryOneOf [TreacheryMatcher]
  | NotTreachery TreacheryMatcher
  deriving stock (Show, Eq, Ord, Data)

instance Not TreacheryMatcher where
  not_ = NotTreachery

instance Semigroup TreacheryMatcher where
  AnyTreachery <> x = x
  x <> AnyTreachery = x
  TreacheryMatches xs <> TreacheryMatches ys = TreacheryMatches (xs <> ys)
  TreacheryMatches xs <> x = TreacheryMatches (x : xs)
  x <> TreacheryMatches xs = TreacheryMatches (x : xs)
  x <> y = TreacheryMatches [x, y]

instance Monoid TreacheryMatcher where
  mempty = AnyTreachery

$(deriveJSON defaultOptions ''TreacheryMatcher)
