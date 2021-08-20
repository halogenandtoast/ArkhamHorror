module Arkham.Types.Investigator.Cards.HarveyWalters where

import Arkham.Prelude

import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype HarveyWalters = HarveyWalters InvestigatorAttrs
  deriving anyclass (HasAbilities env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env HarveyWalters where
  getModifiersFor source target (HarveyWalters attrs) =
    getModifiersFor source target attrs

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

instance (InvestigatorRunner env) => RunMessage env HarveyWalters where
  runMessage msg (HarveyWalters attrs) = HarveyWalters <$> runMessage msg attrs
