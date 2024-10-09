module Arkham.Matcher.Effect where

import Arkham.Card.CardCode
import Arkham.Prelude
import {-# SOURCE #-} Arkham.Target

data EffectMatcher
  = AnyEffect
  | EffectWithCardCode CardCode
  | EffectWithMetaInt Int
  | EffectWithTarget Target
  | EffectMatches [EffectMatcher]
  deriving stock (Show, Eq, Ord, Data)

instance Semigroup EffectMatcher where
  AnyEffect <> x = x
  x <> AnyEffect = x
  EffectMatches xs <> EffectMatches ys = EffectMatches $ xs <> ys
  EffectMatches xs <> x = EffectMatches $ xs <> [x]
  x <> EffectMatches xs = EffectMatches $ x : xs
  x <> y = EffectMatches [x, y]

instance Monoid EffectMatcher where
  mempty = AnyEffect
