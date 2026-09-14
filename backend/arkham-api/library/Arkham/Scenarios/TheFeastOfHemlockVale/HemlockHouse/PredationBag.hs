{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Scenarios.TheFeastOfHemlockVale.HemlockHouse.PredationBag (
  module Arkham.Scenarios.TheFeastOfHemlockVale.HemlockHouse.PredationBag,
  module Arkham.TokenBag,
) where

import Arkham.Prelude
import Arkham.Story.Types
import Arkham.TokenBag
import Data.Aeson (Result (..))
import GHC.Records

initPredationBag :: MonadRandom m => m CustomChaosBag
initPredationBag = initTokenBag [#cultist, #tablet, #elderthing]

predationBag :: StoryAttrs -> CustomChaosBag
predationBag attrs = case fromJSON (storyMeta attrs) of
  Success a -> a
  _ -> error "invalid predation bag"

instance HasField "predationBag" StoryAttrs CustomChaosBag where
  getField = predationBag
