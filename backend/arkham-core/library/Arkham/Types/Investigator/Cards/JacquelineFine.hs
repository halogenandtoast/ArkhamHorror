module Arkham.Types.Investigator.Cards.JacquelineFine where


import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype JacquelineFine = JacquelineFine InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env JacquelineFine where
  getModifiersFor source target (JacquelineFine attrs) =
    getModifiersFor source target attrs

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

instance ActionRunner env => HasActions env JacquelineFine where
  getActions i window (JacquelineFine attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env JacquelineFine where
  runMessage msg (JacquelineFine attrs) =
    JacquelineFine <$> runMessage msg attrs
