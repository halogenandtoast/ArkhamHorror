{- HLINT ignore "Use camelCase" -}
module Arkham.Investigator.Cards.AgathaCrane_Seeker (agathaCrane_Seeker) where

import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Investigator.Cards.AgathaCrane

newtype AgathaCrane_Seeker = AgathaCrane_Seeker InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

agathaCrane_Seeker :: InvestigatorCard AgathaCrane_Seeker
agathaCrane_Seeker =
  investigator AgathaCrane_Seeker Cards.agathaCrane_Seeker
    $ Stats {health = 6, sanity = 8, willpower = 4, intellect = 4, combat = 1, agility = 3}

instance HasAbilities AgathaCrane_Seeker where
  getAbilities (AgathaCrane_Seeker a) = agathaAbilities a

instance HasChaosTokenValue AgathaCrane_Seeker where
  getChaosTokenValue iid token (AgathaCrane_Seeker attrs) = agathaTokenValues iid token attrs

instance RunMessage AgathaCrane_Seeker where
  runMessage msg (AgathaCrane_Seeker attrs) = agathaRunner AgathaCrane_Seeker msg attrs
