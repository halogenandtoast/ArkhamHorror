module Arkham.Types.Investigator.Cards.JacquelineFine where

import Arkham.Prelude

import Arkham.Types.Investigator.Attrs

newtype JacquelineFine = JacquelineFine InvestigatorAttrs
  deriving anyclass (HasAbilities, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

jacquelineFine :: JacquelineFine
jacquelineFine = JacquelineFine $ baseAttrs
  "60401"
  "Jacqueline Fine"
  Mystic
  Stats
    { health = 6
    , sanity = 9
    , willpower = 5
    , intellect = 3
    , combat = 2
    , agility = 2
    }
  [Clairvoyant]

instance (InvestigatorRunner env) => RunMessage env JacquelineFine where
  runMessage msg (JacquelineFine attrs) =
    JacquelineFine <$> runMessage msg attrs
