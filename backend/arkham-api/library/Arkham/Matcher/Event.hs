{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.Event where

import Arkham.Card.CardCode
import Arkham.Card.Id
import Arkham.ClassSymbol
import Arkham.Id
import Arkham.Matcher.Action
import {-# SOURCE #-} Arkham.Matcher.Asset
import Arkham.Matcher.Base
import Arkham.Matcher.Card
import {-# SOURCE #-} Arkham.Matcher.Investigator
import Arkham.Matcher.Location
import Arkham.Matcher.Target
import Arkham.Matcher.Value
import {-# SOURCE #-} Arkham.Modifier
import {-# SOURCE #-} Arkham.Placement
import Arkham.Prelude
import Arkham.Token
import Arkham.Trait (Trait)
import Data.Aeson.TH

instance IsMatcher EventMatcher

data EventMatcher
  = EventWithTitle Text
  | EventWithFullTitle Text Text
  | EventWithId EventId
  | EventWithTrait Trait
  | EventWithClass ClassSymbol
  | EventWithCardId CardId
  | EventWithToken Token
  | EventControlledBy InvestigatorMatcher
  | EventOwnedBy InvestigatorMatcher
  | EventAt LocationMatcher
  | EventWithDoom ValueMatcher
  | EventAttachedToAsset AssetMatcher
  | EventAttachedTo TargetMatcher
  | EventWithModifier ModifierType
  | EventWithoutModifier ModifierType
  | EventIs CardCode
  | EventReady
  | EventCardMatch CardMatcher
  | EventMatches [EventMatcher]
  | EventOneOf [EventMatcher]
  | EnemyEvent EnemyId
  | AnyEvent
  | NotEvent EventMatcher
  | EventWithPlacement Placement
  | ActiveEvent
  | EventWithMetaKey Key
  | EventIsAction ActionMatcher
  deriving stock (Show, Eq, Ord, Data)

instance Not EventMatcher where
  not_ = NotEvent

instance Semigroup EventMatcher where
  EventMatches xs <> EventMatches ys = EventMatches (xs <> ys)
  EventMatches xs <> x = EventMatches (x : xs)
  x <> EventMatches xs = EventMatches (x : xs)
  x <> y = EventMatches [x, y]

$(deriveJSON defaultOptions ''EventMatcher)
