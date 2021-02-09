module Arkham.Types.Investigator.Cards.SisterMary where


import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype SisterMary = SisterMary InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env SisterMary where
  getModifiersFor source target (SisterMary attrs) =
    getModifiersFor source target attrs

sisterMary :: SisterMary
sisterMary = SisterMary $ baseAttrs
  "07001"
  "Sister Mary"
  Guardian
  Stats
    { health = 5
    , sanity = 9
    , willpower = 4
    , intellect = 2
    , combat = 3
    , agility = 3
    }
  [Believer, Blessed]

instance ActionRunner env => HasActions env SisterMary where
  getActions i window (SisterMary attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env SisterMary where
  runMessage msg (SisterMary attrs) = SisterMary <$> runMessage msg attrs
