module Arkham.Types.Investigator.Cards.PatriceHathaway where

import Arkham.Prelude

import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype PatriceHathaway = PatriceHathaway InvestigatorAttrs
  deriving anyclass (HasAbilities, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

patriceHathaway :: PatriceHathaway
patriceHathaway = PatriceHathaway $ baseAttrs
  "06005"
  "Patrice Hathaway"
  Survivor
  Stats
    { health = 7
    , sanity = 7
    , willpower = 4
    , intellect = 2
    , combat = 2
    , agility = 2
    }
  [Performer, Cursed]

instance (InvestigatorRunner env) => RunMessage env PatriceHathaway where
  runMessage msg (PatriceHathaway attrs) =
    PatriceHathaway <$> runMessage msg attrs
