{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -O0 #-}

module Arkham.Helpers.Window.Damage where

import Arkham.Id
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Arkham.Window
import Arkham.Window qualified as Window
import Data.Map.Monoidal.Strict qualified as MonoidalMap
import Data.Map.Strict qualified as Map

healedAmount :: [Window] -> Int
healedAmount = sum . mapMaybe toHealedAmount
 where
  toHealedAmount = \case
    (windowType -> Healed _ _ _ n) -> Just n
    _ -> Nothing

healedInvestigator :: [Window] -> InvestigatorId
healedInvestigator [] = error "invalid call"
healedInvestigator ((windowType -> Window.Healed _ (InvestigatorTarget iid) _ _) : _) = iid
healedInvestigator (_ : xs) = healedInvestigator xs

damagedInvestigator :: [Window] -> InvestigatorId
damagedInvestigator [] = error "no damaged investigator"
damagedInvestigator ((windowType -> Window.WouldTakeDamageOrHorror _ (InvestigatorTarget iid) _ _) : _) = iid
damagedInvestigator (_ : xs) = damagedInvestigator xs

dealtDamage :: [Window] -> Int
dealtDamage [] = 0
dealtDamage ((windowType -> Window.WouldTakeDamageOrHorror _ _ n _) : _) = n
dealtDamage (_ : xs) = dealtDamage xs

dealtHorror :: [Window] -> Int
dealtHorror [] = 0
dealtHorror ((windowType -> Window.WouldTakeDamageOrHorror _ _ _ n) : _) = n
dealtHorror (_ : xs) = dealtDamage xs

damagedAsset :: [Window] -> AssetId
damagedAsset = \case
  [] -> error "Expected DealtDamageOrHorro to asset window"
  ((windowType -> Window.DealtDamage _ _ (AssetTarget aid) _) : _) -> aid
  ((windowType -> Window.DealtHorror _ (AssetTarget aid) _) : _) -> aid
  (_ : rest) -> damagedAsset rest

getDamageSource :: HasCallStack => [Window] -> Source
getDamageSource = \case
  [] -> error "No damage"
  ((windowType -> Window.DealtDamage source _ _ _) : _) -> source
  ((windowType -> Window.DealtExcessDamage source _ _ _) : _) -> source
  (_ : rest) -> getDamageSource rest

getDamageSourceEnemy :: HasCallStack => [Window] -> EnemyId
getDamageSourceEnemy ws = case (getDamageSource ws).enemy of
  Nothing -> error "Source was not enemy"
  Just eid -> eid

getDamageOrHorrorSource :: HasCallStack => [Window] -> Source
getDamageOrHorrorSource = \case
  [] -> error "No damage"
  ((windowType -> Window.DealtDamage source _ _ _) : _) -> source
  ((windowType -> Window.DealtHorror source _ _) : _) -> source
  ((windowType -> Window.DealtExcessDamage source _ _ _) : _) -> source
  (_ : rest) -> getDamageOrHorrorSource rest

getTotalDamageAmounts :: Targetable target => target -> [Window] -> Map Source (Int, Int)
getTotalDamageAmounts target =
  Map.map (bimap getSum getSum) . MonoidalMap.getMonoidalMap . foldMap \case
    (windowType -> Window.DealtDamage source _ (isTarget target -> True) d) -> MonoidalMap.singleton source (Sum d, Sum 0)
    (windowType -> Window.DealtHorror source (isTarget target -> True) h) -> MonoidalMap.singleton source (Sum 0, Sum h)
    (windowType -> Window.DealtExcessDamage source _ (isTarget target -> True) d) -> MonoidalMap.singleton source (Sum d, Sum 0)
    (windowType -> Window.WouldTakeDamage source (isTarget target -> True) d _) -> MonoidalMap.singleton source (Sum d, Sum 0)
    (windowType -> Window.WouldTakeHorror source (isTarget target -> True) h) -> MonoidalMap.singleton source (Sum 0, Sum h)
    _ -> mempty

getTotalDamage :: [Window] -> Int
getTotalDamage ((windowType -> Window.DealtDamage _ _ _ n) : rest) = n + getTotalDamage rest
getTotalDamage ((windowType -> Window.TakeDamage _ _ _ n) : rest) = n + getTotalDamage rest
getTotalDamage (_ : rest) = getTotalDamage rest
getTotalDamage [] = 0
