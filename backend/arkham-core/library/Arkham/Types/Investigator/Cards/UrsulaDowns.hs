module Arkham.Types.Investigator.Cards.UrsulaDowns where


import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype UrsulaDowns = UrsulaDowns InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env UrsulaDowns where
  getModifiersFor source target (UrsulaDowns attrs) =
    getModifiersFor source target attrs

ursulaDowns :: UrsulaDowns
ursulaDowns = UrsulaDowns $ baseAttrs
  "04002"
  "Ursula Downs"
  Seeker
  Stats
    { health = 7
    , sanity = 7
    , willpower = 3
    , intellect = 4
    , combat = 1
    , agility = 4
    }
  [Wayfarer]

instance ActionRunner env => HasActions env UrsulaDowns where
  getActions i window (UrsulaDowns attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env UrsulaDowns where
  runMessage msg (UrsulaDowns attrs) = UrsulaDowns <$> runMessage msg attrs
