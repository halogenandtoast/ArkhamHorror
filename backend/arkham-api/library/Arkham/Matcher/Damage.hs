{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.Damage where

import Arkham.Prelude
import Data.Aeson.TH

data DamageTypeMatcher = IsDirectDamage | IsNonDirectDamage | AnyDamageType
  deriving stock (Show, Eq, Ord, Data)

data DamageEffectMatcher
  = AttackDamageEffect
  | NonAttackDamageEffect
  | AnyDamageEffect
  deriving stock (Show, Eq, Ord, Data)

mconcat
  [ deriveJSON defaultOptions ''DamageEffectMatcher
  , deriveJSON defaultOptions ''DamageTypeMatcher
  ]
