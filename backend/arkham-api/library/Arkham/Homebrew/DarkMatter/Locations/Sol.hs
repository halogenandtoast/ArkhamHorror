module Arkham.Homebrew.DarkMatter.Locations.Sol (sol) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Sol = Sol LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | The sun itself; the Ritual of the Sun story attaches here.

Sol prints no connections, and no Starfall location prints its symbol, so the
only way to be at a location connecting to Sol is to park a [[Starship]] on it —
which is exactly what Ritual of the Sun's "investigators at a connecting
location" and resolution 2's "a dark cloud [...] surrounding the entire ship"
describe. Nobody can ever stand on Sol itself.
-}
sol :: LocationCard Sol
sol = symbolLabel $ location Sol Cards.sol 9 (Static 0)

-- | "Forced - After you enter this location: You are immediately killed."
instance HasAbilities Sol where
  getAbilities (Sol a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ Enters #after You (be a)

instance RunMessage Sol where
  runMessage msg l@(Sol attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      kill attrs iid
      pure l
    _ -> Sol <$> liftRunMessage msg attrs
