module Arkham.Types.Investigator.Cards.CalvinWright where

import Arkham.Prelude

import Arkham.Types.Investigator.Attrs

newtype CalvinWright = CalvinWright InvestigatorAttrs
  deriving anyclass (HasAbilities, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

calvinWright :: CalvinWright
calvinWright = CalvinWright $ baseAttrs
  "04005"
  "Calvin Wright"
  Survivor
  Stats
    { health = 6
    , sanity = 6
    , willpower = 0
    , intellect = 0
    , combat = 0
    , agility = 0
    }
  [Cursed, Drifter]

instance InvestigatorRunner env => RunMessage env CalvinWright where
  runMessage msg (CalvinWright attrs) = CalvinWright <$> runMessage msg attrs
