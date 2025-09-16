{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Classes.HasGame where

import Arkham.Action
import Arkham.Cost
import {-# SOURCE #-} Arkham.Game.Base
import Arkham.Id
import Arkham.Prelude
import Arkham.Query
import Arkham.Queue
import Arkham.Source
import Arkham.Window
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.GADT.Compare
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.GADT.Show.TH (deriveGShow)
import Data.Typeable

cached :: forall m v. HasGame m => CacheKey v -> m v -> m v
cached k build = case getCache @m of
  GameCache wc -> wc k build

class Monad m => HasGame m where
  getGame :: m Game
  getCache :: GameCache m

instance HasGame m => HasGame (QueueT msg m) where
  getGame = lift getGame
  getCache = GameCache \_ build -> build

instance Monad m => HasGame (ReaderT Game m) where
  getGame = ask
  getCache = GameCache \_ build -> build

instance HasGame m => HasGame (StateT s m) where
  getGame = lift getGame
  getCache = GameCache \_ build -> build

instance HasGame m => HasGame (MaybeT m) where
  getGame = lift getGame
  getCache = GameCache \_ build -> build

instance (Monoid w, HasGame m) => HasGame (WriterT w m) where
  getGame = lift getGame
  getCache = GameCache \_ build -> build

newtype GameCache m = GameCache {withCache :: forall v. CacheKey v -> m v -> m v}

data CacheKey v where
  CanAffordCostKey
    :: InvestigatorId -> Source -> [Action] -> [Window] -> Bool -> Cost -> CacheKey Bool
  CanMoveToLocationKey :: InvestigatorId -> Source -> LocationId -> CacheKey Bool
  CanMoveToLocationsKey_ :: InvestigatorId -> Source -> [LocationId] -> CacheKey [LocationId]
  CanMoveToLocationsKey :: InvestigatorId -> Source -> CacheKey [LocationId]
  SelectKey :: Typeable a => SomeQuery a -> CacheKey [a]
  ExistKey :: Typeable a => SomeQuery a -> CacheKey Bool

deriving stock instance Show (CacheKey v)

deriveGEq ''SomeQuery
deriveGCompare ''SomeQuery
deriveGShow ''SomeQuery

deriveGShow ''CacheKey

-- Equality on keys (must compare fields!)
instance GEq CacheKey where
  geq
    (CanAffordCostKey a1 b1 c1 d1 e1 f1)
    (CanAffordCostKey a2 b2 c2 d2 e2 f2)
      | (a1, b1, c1, d1, e1, f1) == (a2, b2, c2, d2, e2, f2) = Just Refl
      | otherwise = Nothing
  geq
    (CanMoveToLocationKey a1 b1 c1)
    (CanMoveToLocationKey a2 b2 c2)
      | (a1, b1, c1) == (a2, b2, c2) = Just Refl
      | otherwise = Nothing
  geq
    (CanMoveToLocationsKey_ a1 b1 c1)
    (CanMoveToLocationsKey_ a2 b2 c2)
      | (a1, b1, c1) == (a2, b2, c2) = Just Refl
      | otherwise = Nothing
  geq
    (CanMoveToLocationsKey a1 b1)
    (CanMoveToLocationsKey a2 b2)
      | (a1, b1) == (a2, b2) = Just Refl
      | otherwise = Nothing
  geq (SelectKey (q1 :: SomeQuery a)) (SelectKey (q2 :: SomeQuery b))
    | Just Refl <- eqT @a @b, q1 == q2 = Just Refl -- requires Eq (SomeQuery a)
    | otherwise = Nothing
  geq (ExistKey (q1 :: SomeQuery a)) (ExistKey (q2 :: SomeQuery b))
    | Just Refl <- eqT @a @b, q1 == q2 = Just Refl -- requires Eq (SomeQuery a)
    | otherwise = Nothing
  geq _ _ = Nothing

-- Total ordering on keys (used by DMap’s balanced tree)
instance GCompare CacheKey where
  gcompare k1 k2 = case (k1, k2) of
    (CanAffordCostKey a1 b1 c1 d1 e1 f1, CanAffordCostKey a2 b2 c2 d2 e2 f2) ->
      compareTuple (a1, b1, c1, d1, e1, f1) (a2, b2, c2, d2, e2, f2)
    (CanAffordCostKey {}, _) -> GLT
    (CanMoveToLocationKey {}, CanAffordCostKey {}) -> GGT
    (CanMoveToLocationKey a1 b1 c1, CanMoveToLocationKey a2 b2 c2) ->
      compareTuple (a1, b1, c1) (a2, b2, c2)
    (CanMoveToLocationKey {}, _) -> GLT
    (CanMoveToLocationsKey_ {}, CanAffordCostKey {}) -> GGT
    (CanMoveToLocationsKey_ {}, CanMoveToLocationKey {}) -> GGT
    (CanMoveToLocationsKey_ a1 b1 c1, CanMoveToLocationsKey_ a2 b2 c2) ->
      compareTuple (a1, b1, c1) (a2, b2, c2)
    (CanMoveToLocationsKey_ {}, _) -> GLT
    (CanMoveToLocationsKey {}, CanAffordCostKey {}) -> GGT
    (CanMoveToLocationsKey {}, CanMoveToLocationKey {}) -> GGT
    (CanMoveToLocationsKey {}, CanMoveToLocationsKey_ {}) -> GGT
    (CanMoveToLocationsKey a1 b1, CanMoveToLocationsKey a2 b2) ->
      compareTuple (a1, b1) (a2, b2)
    (CanMoveToLocationsKey {}, _) -> GLT
    (SelectKey (q1 :: SomeQuery a), SelectKey (q2 :: SomeQuery b)) | Just Refl <- eqT @a @b ->
      case compare q1 q2 of -- requires Ord (SomeQuery a)
        LT -> GLT
        EQ -> GEQ
        GT -> GGT
    (SelectKey {}, _) -> GGT
    (ExistKey (q1 :: SomeQuery a), ExistKey (q2 :: SomeQuery b)) | Just Refl <- eqT @a @b ->
      case compare q1 q2 of -- requires Ord (SomeQuery a)
        LT -> GLT
        EQ -> GEQ
        GT -> GGT
    (ExistKey {}, SelectKey {}) -> GLT
    (ExistKey {}, _) -> GGT
   where
    -- Helper: lift ordinary Ord tuple comparison to GCompare’s 3-way
    compareTuple :: Ord t => t -> t -> GOrdering a a
    compareTuple x y = case compare x y of
      LT -> GLT
      EQ -> GEQ
      GT -> GGT
