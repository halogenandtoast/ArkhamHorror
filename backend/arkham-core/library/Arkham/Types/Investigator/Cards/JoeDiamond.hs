module Arkham.Types.Investigator.Cards.JoeDiamond where

import Arkham.Prelude

import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype JoeDiamond = JoeDiamond InvestigatorAttrs
  deriving anyclass (HasAbilities, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

joeDiamond :: JoeDiamond
joeDiamond = JoeDiamond $ baseAttrs
  "05002"
  "Joe Diamond"
  Seeker
  Stats
    { health = 8
    , sanity = 6
    , willpower = 2
    , intellect = 4
    , combat = 4
    , agility = 2
    }
  [Detective]

instance (InvestigatorRunner env) => RunMessage env JoeDiamond where
  runMessage msg (JoeDiamond attrs) = JoeDiamond <$> runMessage msg attrs
