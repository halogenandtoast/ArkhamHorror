{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.Source where

import Arkham.Card.CardType
import Arkham.Matcher.Asset
import Arkham.Matcher.Card
import Arkham.Matcher.Enemy
import Arkham.Matcher.Event
import Arkham.Matcher.Investigator
import Arkham.Matcher.Location
import Arkham.Matcher.Treachery
import Arkham.Prelude
import {-# SOURCE #-} Arkham.Source
import Arkham.Trait (Trait (..))
import Data.Aeson.TH
import GHC.OverloadedLabels

data SourceMatcher
  = SourceWithTrait Trait
  | SourceIsEnemyAttack EnemyMatcher
  | SourceIsTreacheryEffect TreacheryMatcher
  | SourceIsAsset AssetMatcher
  | SourceIsEvent EventMatcher
  | SourceIsLocation LocationMatcher
  | SourceIsEnemy EnemyMatcher
  | EncounterCardSource
  | SourceMatchesAny [SourceMatcher]
  | SourceOwnedBy InvestigatorMatcher
  | SourceIsType CardType
  | SourceIsPlayerCard
  | SourceIsPlayerCardAbility
  | AnySource
  | SourceIsCancelable SourceMatcher
  | SourceMatches [SourceMatcher]
  | NotSource SourceMatcher
  | SourceIs Source
  | SourceWithCard CardMatcher
  | SourceIsCardEffect
  deriving stock (Show, Eq, Ord, Data)

instance IsLabel "spell" SourceMatcher where
  fromLabel = SourceWithTrait Spell

instance IsLabel "relic" SourceMatcher where
  fromLabel = SourceWithTrait Relic

pattern AnyCancellableSource :: SourceMatcher
pattern AnyCancellableSource <- SourceIsCancelable AnySource
  where
    AnyCancellableSource = SourceIsCancelable AnySource

instance IsLabel "investigator" SourceMatcher where
  fromLabel = SourceIsType InvestigatorType

instance IsLabel "any" SourceMatcher where
  fromLabel = AnySource

instance Semigroup SourceMatcher where
  AnySource <> x = x
  x <> AnySource = x
  SourceMatches xs <> SourceMatches ys = SourceMatches $ xs <> ys
  SourceMatches xs <> x = SourceMatches $ xs <> [x]
  x <> SourceMatches xs = SourceMatches $ x : xs
  x <> y = SourceMatches [x, y]

instance Monoid SourceMatcher where
  mempty = AnySource

$(deriveJSON defaultOptions ''SourceMatcher)
