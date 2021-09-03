module Arkham.Types.Investigator.Cards.LeoAnderson where

import Arkham.Prelude

import Arkham.Types.Investigator.Attrs

newtype LeoAnderson = LeoAnderson InvestigatorAttrs
  deriving anyclass (HasAbilities, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

leoAnderson :: LeoAnderson
leoAnderson = LeoAnderson $ baseAttrs
  "04001"
  "Leo Anderson"
  Guardian
  Stats
    { health = 8
    , sanity = 6
    , willpower = 4
    , intellect = 3
    , combat = 4
    , agility = 1
    }
  [Veteran, Wayfarer]

instance (InvestigatorRunner env) => RunMessage env LeoAnderson where
  runMessage msg (LeoAnderson attrs) = LeoAnderson <$> runMessage msg attrs
