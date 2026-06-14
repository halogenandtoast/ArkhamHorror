module Arkham.Location.Cards.SunkenStairway (sunkenStairway) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted

newtype SunkenStairway = SunkenStairway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sunkenStairway :: LocationCard SunkenStairway
sunkenStairway = location SunkenStairway Cards.sunkenStairway 0 (Static 2)

instance RunMessage SunkenStairway where
  runMessage msg (SunkenStairway attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      -- Forced: After this location is revealed, put the set-aside Undersea
      -- Vault into play in a row directly above or below this one.
      -- TODO: let the players choose above (Pos col (row + 1)) vs below; for now
      -- we always place directly below.
      for_ (locationPosition attrs) \(Pos row col) -> do
        card <- getSetAsideCard Cards.underseaVault
        placeLocationInGrid_ (Pos (row + 1) col) card
      SunkenStairway <$> liftRunMessage msg attrs
    _ -> SunkenStairway <$> liftRunMessage msg attrs
