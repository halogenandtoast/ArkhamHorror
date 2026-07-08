{-# LANGUAGE TemplateHaskell #-}

module Arkham.UltimatumsAndBoons.Types where

import Arkham.Prelude
import Data.Aeson.TH

data Boon
  = BoonOfTheAncients
  | BoonOfAthena
  | BoonOfDestiny
  | BoonOfHades
  | BoonOfHermes
  | BoonOfThoth
  | BoonOfOsiris
  | BoonOfTheMorrigan
  | BoonOfPersephone
  | BoonOfTheExplorer
  | BoonOfTheChild
  deriving stock (Eq, Show, Ord, Enum, Bounded, Data)

data Ultimatum
  = UltimatumOfAgony
  | UltimatumOfBrokenPromises
  | UltimatumOfTheBrokenVeil
  | UltimatumOfChaos
  | UltimatumOfDisaster
  | UltimatumOfDread
  | UltimatumOfFailrue
  | UltimatumOfFinality
  | UltimatumOfForbiddenKnowledge
  | UltimatumOfHardship
  | UltimatumOfTheHighlander
  | UltimatumOfInduction
  | UltimatumOfOrthodoxy
  | UltimatumOfTheScream
  | UltimatumOfSurvival
  | UltimatumOfUltimatums
  | UltimatumOfExile
  | UltimatumOfTheSpiral
  | UltimatumOfMalevolence
  deriving stock (Eq, Show, Ord, Data)

data UltimatumOrBoon
  = Ultimatum Ultimatum
  | Boon Boon
  deriving stock (Eq, Show, Ord, Data)

-- | Flat constructor name of the underlying entry (e.g. "BoonOfHades");
-- doubles as the wire representation and synthetic card code.
variantName :: UltimatumOrBoon -> Text
variantName = \case
  Ultimatum u -> tshow u
  Boon b -> tshow b

mconcat
  [ deriveJSON defaultOptions ''Boon
  , deriveJSON defaultOptions ''Ultimatum
  , deriveJSON defaultOptions ''UltimatumOrBoon
  ]
