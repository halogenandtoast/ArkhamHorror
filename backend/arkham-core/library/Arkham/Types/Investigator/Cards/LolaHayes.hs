module Arkham.Types.Investigator.Cards.LolaHayes where

import Arkham.Prelude

import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype LolaHayes = LolaHayes InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env LolaHayes where
  getModifiersFor source target (LolaHayes attrs) =
    getModifiersFor source target attrs

lolaHayes :: LolaHayes
lolaHayes = LolaHayes $ baseAttrs
  "03006"
  "Lola Hayes"
  Neutral
  Stats
    { health = 6
    , sanity = 6
    , willpower = 3
    , intellect = 3
    , combat = 3
    , agility = 3
    }
  [Performer]

instance HasActions env LolaHayes where
  getActions i window (LolaHayes attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env LolaHayes where
  runMessage msg (LolaHayes attrs) = LolaHayes <$> runMessage msg attrs
