module Arkham.Types.Investigator.Cards.NathanielCho where

import Arkham.Prelude

import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype NathanielCho = NathanielCho InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env NathanielCho where
  getModifiersFor source target (NathanielCho attrs) =
    getModifiersFor source target attrs

nathanielCho :: NathanielCho
nathanielCho = NathanielCho $ baseAttrs
  "60101"
  "Nathaniel Cho"
  Guardian
  Stats
    { health = 9
    , sanity = 6
    , willpower = 3
    , intellect = 2
    , combat = 5
    , agility = 2
    }
  [Criminal, Warden]

instance InvestigatorRunner env => HasAbilities env NathanielCho where
  getAbilities i window (NathanielCho attrs) = getAbilities i window attrs

instance (InvestigatorRunner env) => RunMessage env NathanielCho where
  runMessage msg (NathanielCho attrs) = NathanielCho <$> runMessage msg attrs
