module Arkham.Types.Investigator.Cards.MarieLambeau where

import Arkham.Prelude

import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype MarieLambeau = MarieLambeau InvestigatorAttrs
  deriving anyclass (HasAbilities, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

marieLambeau :: MarieLambeau
marieLambeau = MarieLambeau $ baseAttrs
  "05006"
  "Marie Lambeau"
  Mystic
  Stats
    { health = 6
    , sanity = 8
    , willpower = 4
    , intellect = 4
    , combat = 1
    , agility = 3
    }
  [Performer, Sorcerer]

instance (InvestigatorRunner env) => RunMessage env MarieLambeau where
  runMessage msg (MarieLambeau attrs) = MarieLambeau <$> runMessage msg attrs
