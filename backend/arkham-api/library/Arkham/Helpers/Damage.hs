module Arkham.Helpers.Damage where

import Arkham.DamageEffect
import Arkham.Matcher qualified as Matcher
import Arkham.Prelude
import Arkham.Strategy

damageEffectMatches :: Monad m => DamageEffect -> Matcher.DamageEffectMatcher -> m Bool
damageEffectMatches a = \case
  Matcher.AnyDamageEffect -> pure True
  Matcher.AttackDamageEffect -> pure $ a == AttackDamageEffect
  Matcher.NonAttackDamageEffect -> pure $ a == NonAttackDamageEffect

damageTypeMatches :: DamageStrategy -> Matcher.DamageTypeMatcher -> Bool
damageTypeMatches strategy = \case
  Matcher.IsDirectDamage -> isDirectStrategy
  Matcher.IsNonDirectDamage -> not isDirectStrategy
  Matcher.AnyDamageType -> True
 where
  isDirectStrategy = case strategy of
    DamageDirect -> True
    DamageAny -> False
    DamageAssetsFirst -> False
    DamageFirst _ -> False
    SingleTarget -> False
    DamageEvenly -> False
    DamageFromHastur -> False
