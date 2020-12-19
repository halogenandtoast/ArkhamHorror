module Arkham.Types.EffectMetadata
  ( EffectMetadata(..)
  )
where

import Arkham.Types.Card.CardCode
import Arkham.Types.Modifier
import ClassyPrelude
import Data.Aeson

data EffectMetadata a = EffectInt Int | EffectMessages [a] | EffectModifiers [Modifier] | EffectCardCode CardCode
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
