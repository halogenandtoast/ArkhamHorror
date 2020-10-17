{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.HarveyWalters where

import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype HarveyWalters = HarveyWalters Attrs
  deriving newtype (Show, ToJSON, FromJSON)

harveyWalters :: HarveyWalters
harveyWalters = HarveyWalters $ baseAttrs
  "60201"
  "Harvey Walters"
  Seeker
  Stats
    { health = 7
    , sanity = 8
    , willpower = 4
    , intellect = 5
    , combat = 1
    , agility = 2
    }
  [Miskatonic]

instance ActionRunner env => HasActions env HarveyWalters where
  getActions i window (HarveyWalters attrs) = getActions i window attrs

instance (InvestigatorRunner Attrs env) => RunMessage env HarveyWalters where
  runMessage msg i@(HarveyWalters attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid | iid == investigatorId -> pure i
    _ -> HarveyWalters <$> runMessage msg attrs
