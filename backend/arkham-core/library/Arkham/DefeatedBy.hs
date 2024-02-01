{-# LANGUAGE TemplateHaskell #-}

module Arkham.DefeatedBy where

import Arkham.Prelude

import Arkham.Source
import Data.Aeson.TH

data DefeatedBy
  = DefeatedByHorror Source
  | DefeatedByDamage Source
  | DefeatedByDamageAndHorror Source
  | DefeatedByOther Source
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NoThunks)

wasDefeatedByHorror :: DefeatedBy -> Bool
wasDefeatedByHorror = \case
  DefeatedByHorror _ -> True
  DefeatedByDamageAndHorror _ -> True
  _ -> False

wasDefeatedByDamage :: DefeatedBy -> Bool
wasDefeatedByDamage = \case
  DefeatedByDamage _ -> True
  DefeatedByDamageAndHorror _ -> True
  _ -> False

wasDefeatedByOther :: DefeatedBy -> Bool
wasDefeatedByOther = \case
  DefeatedByOther _ -> True
  _ -> False

defeatedBySource :: DefeatedBy -> Source
defeatedBySource = \case
  DefeatedByHorror source -> source
  DefeatedByDamage source -> source
  DefeatedByDamageAndHorror source -> source
  DefeatedByOther source -> source

$(deriveJSON defaultOptions ''DefeatedBy)
