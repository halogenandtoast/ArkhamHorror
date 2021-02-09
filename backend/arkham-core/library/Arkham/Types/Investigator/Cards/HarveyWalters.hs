module Arkham.Types.Investigator.Cards.HarveyWalters where


import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype HarveyWalters = HarveyWalters InvestigatorAttrs
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

instance ActionRunner env => HasActions env HarveyWalters where
  getActions i window (HarveyWalters attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env HarveyWalters where
  runMessage msg (HarveyWalters attrs) = HarveyWalters <$> runMessage msg attrs
