module Arkham.Types.Investigator.Cards.FatherMateo where

import Arkham.Prelude

import Arkham.Types.Investigator.Attrs

newtype FatherMateo = FatherMateo InvestigatorAttrs
  deriving anyclass (HasAbilities, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

fatherMateo :: FatherMateo
fatherMateo = FatherMateo $ baseAttrs
  "04004"
  "Father Mateo"
  Mystic
  Stats
    { health = 6
    , sanity = 8
    , willpower = 4
    , intellect = 3
    , combat = 2
    , agility = 3
    }
  [Believer, Warden]

instance (InvestigatorRunner env) => RunMessage env FatherMateo where
  runMessage msg (FatherMateo attrs) = FatherMateo <$> runMessage msg attrs
