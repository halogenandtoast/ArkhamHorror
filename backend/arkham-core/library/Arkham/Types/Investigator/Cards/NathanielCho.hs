module Arkham.Types.Investigator.Cards.NathanielCho where


import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
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

instance ActionRunner env => HasActions env NathanielCho where
  getActions i window (NathanielCho attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env NathanielCho where
  runMessage msg (NathanielCho attrs) = NathanielCho <$> runMessage msg attrs
