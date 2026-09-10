{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Scenarios.TheDreamEaters.WakingNightmare.InfestationBag (
  module Arkham.Scenarios.TheDreamEaters.WakingNightmare.InfestationBag,
  module Arkham.TokenBag,
) where

import Arkham.Prelude
import Arkham.Story.Types
import Arkham.TokenBag
import Data.Aeson (Result (..))
import GHC.Records

initInfestationBag :: MonadRandom m => m CustomChaosBag
initInfestationBag = initTokenBag [#skull, #tablet, #tablet, #tablet, #tablet, #cultist, #cultist]

infestationBag :: StoryAttrs -> CustomChaosBag
infestationBag attrs = case fromJSON (storyMeta attrs) of
  Success a -> a
  _ -> error "invalid infestation bag"

instance HasField "infestationBag" StoryAttrs CustomChaosBag where
  getField = infestationBag
