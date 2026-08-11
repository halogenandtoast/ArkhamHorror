module Arkham.Homebrew.DarkMatter.Locations.Sol (sol) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Sol = Sol LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | The sun itself; the Ritual of the Sun story attaches here.
sol :: LocationCard Sol
sol = location Sol Cards.sol 9 (Static 0)

-- | "Forced - After you enter this location: You are immediately killed."
instance HasAbilities Sol where
  getAbilities (Sol a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ Enters #after You (be a)

instance RunMessage Sol where
  runMessage msg l@(Sol attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ InvestigatorKilled (toSource attrs) iid
      pure l
    _ -> Sol <$> liftRunMessage msg attrs
