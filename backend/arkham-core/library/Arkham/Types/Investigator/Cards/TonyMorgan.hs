{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.TonyMorgan where

import Arkham.Import

import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype TonyMorgan = TonyMorgan Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance HasModifiersFor env TonyMorgan where
  getModifiersFor source target (TonyMorgan attrs) =
    getModifiersFor source target attrs

tonyMorgan :: TonyMorgan
tonyMorgan = TonyMorgan $ baseAttrs
  "06003"
  "Tony Morgan"
  Rogue
  Stats
    { health = 9
    , sanity = 5
    , willpower = 2
    , intellect = 3
    , combat = 5
    , agility = 2
    }
  [Criminal, Hunter]

instance ActionRunner env => HasActions env TonyMorgan where
  getActions i window (TonyMorgan attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env TonyMorgan where
  runMessage msg (TonyMorgan attrs) = TonyMorgan <$> runMessage msg attrs
