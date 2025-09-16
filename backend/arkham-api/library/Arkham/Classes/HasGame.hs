{-# LANGUAGE TemplateHaskell #-}

module Arkham.Classes.HasGame where

import Arkham.Action
import Arkham.Cost
import {-# SOURCE #-} Arkham.Game.Base
import Arkham.Id
import Arkham.Prelude
import Arkham.Queue
import Arkham.Source
import Arkham.Window
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.GADT.Show.TH (deriveGShow)

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

deriving stock instance Show (CacheKey v)

deriveGEq ''CacheKey
deriveGCompare ''CacheKey
deriveGShow ''CacheKey
deriveArgDict ''CacheKey
