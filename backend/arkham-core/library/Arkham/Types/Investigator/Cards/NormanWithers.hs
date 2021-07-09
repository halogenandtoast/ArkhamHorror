module Arkham.Types.Investigator.Cards.NormanWithers where

import Arkham.Prelude

import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype NormanWithers = NormanWithers InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env NormanWithers where
  getModifiersFor source target (NormanWithers attrs) =
    getModifiersFor source target attrs

normanWithers :: NormanWithers
normanWithers = NormanWithers $ baseAttrs
  "98007"
  "Norman Withers"
  Seeker
  Stats
    { health = 6
    , sanity = 8
    , willpower = 4
    , intellect = 5
    , combat = 2
    , agility = 1
    }
  [Miskatonic]

instance HasActions env NormanWithers where
  getActions i window (NormanWithers attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env NormanWithers where
  runMessage msg (NormanWithers attrs) = NormanWithers <$> runMessage msg attrs
