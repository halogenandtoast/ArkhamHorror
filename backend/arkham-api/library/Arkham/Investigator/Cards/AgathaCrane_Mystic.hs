{- HLINT ignore "Use camelCase" -}
module Arkham.Investigator.Cards.AgathaCrane_Mystic (agathaCrane_Mystic) where

import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Investigator.Cards.AgathaCrane

newtype AgathaCrane_Mystic = AgathaCrane_Mystic InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

agathaCrane_Mystic :: InvestigatorCard AgathaCrane_Mystic
agathaCrane_Mystic =
  investigator AgathaCrane_Mystic Cards.agathaCrane_Mystic
    $ Stats {health = 6, sanity = 8, willpower = 4, intellect = 4, combat = 1, agility = 3}

instance HasAbilities AgathaCrane_Mystic where
  getAbilities (AgathaCrane_Mystic a) = agathaAbilities a

instance HasChaosTokenValue AgathaCrane_Mystic where
  getChaosTokenValue iid token (AgathaCrane_Mystic attrs) = agathaTokenValues iid token attrs

instance RunMessage AgathaCrane_Mystic where
  runMessage msg (AgathaCrane_Mystic attrs) = agathaRunner AgathaCrane_Mystic msg attrs
