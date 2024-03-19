module Arkham.Location.Cards.ForsakenTowerOfTheQueenOfNight
  ( forsakenTowerOfTheQueenOfNight
  , ForsakenTowerOfTheQueenOfNight(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ForsakenTowerOfTheQueenOfNight = ForsakenTowerOfTheQueenOfNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forsakenTowerOfTheQueenOfNight :: LocationCard ForsakenTowerOfTheQueenOfNight
forsakenTowerOfTheQueenOfNight = location ForsakenTowerOfTheQueenOfNight Cards.forsakenTowerOfTheQueenOfNight 3 (PerPlayer 1)

instance HasAbilities ForsakenTowerOfTheQueenOfNight where
  getAbilities (ForsakenTowerOfTheQueenOfNight attrs) =
    extendRevealed attrs []

instance RunMessage ForsakenTowerOfTheQueenOfNight where
  runMessage msg (ForsakenTowerOfTheQueenOfNight attrs) = runQueueT $ case msg of
    _ -> ForsakenTowerOfTheQueenOfNight <$> lift (runMessage msg attrs)
