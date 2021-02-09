module Arkham.Types.Investigator.Cards.PrestonFairmont where


import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype PrestonFairmont = PrestonFairmont InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env PrestonFairmont where
  getModifiersFor source target (PrestonFairmont attrs) =
    getModifiersFor source target attrs

prestonFairmont :: PrestonFairmont
prestonFairmont = PrestonFairmont $ baseAttrs
  "05003"
  "Preston Fairmont"
  Rogue
  Stats
    { health = 7
    , sanity = 7
    , willpower = 1
    , intellect = 1
    , combat = 1
    , agility = 1
    }
  [SilverTwilight, Socialite]

instance ActionRunner env => HasActions env PrestonFairmont where
  getActions i window (PrestonFairmont attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env PrestonFairmont where
  runMessage msg (PrestonFairmont attrs) =
    PrestonFairmont <$> runMessage msg attrs
