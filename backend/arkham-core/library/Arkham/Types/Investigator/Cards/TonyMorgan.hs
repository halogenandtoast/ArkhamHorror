module Arkham.Types.Investigator.Cards.TonyMorgan where

import Arkham.Prelude

import Arkham.Types.Investigator.Attrs

newtype TonyMorgan = TonyMorgan InvestigatorAttrs
  deriving anyclass (HasAbilities, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

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

instance (InvestigatorRunner env) => RunMessage env TonyMorgan where
  runMessage msg (TonyMorgan attrs) = TonyMorgan <$> runMessage msg attrs
