module Arkham.Location.Cards.QuietHalls_131 (quietHalls_131) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait

newtype QuietHalls_131 = QuietHalls_131 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quietHalls_131 :: LocationCard QuietHalls_131
quietHalls_131 =
  locationWith QuietHalls_131 Cards.quietHalls_131 3 (Static 0)
    $ (connectedMatchersL <>~ [LocationWithTrait SecondFloor])
    . (revealedConnectedMatchersL <>~ [LocationWithTrait SecondFloor])

instance HasAbilities QuietHalls_131 where
  getAbilities (QuietHalls_131 a) =
    extendRevealed1 a
      $ restricted
        a
        1
        (Here <> notExists UnrevealedLocation <> notExists LocationWithAnyClues)
        actionAbility

instance RunMessage QuietHalls_131 where
  runMessage msg l@(QuietHalls_131 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeClues (attrs.ability 1) attrs =<< getPlayerCount
      pure l
    _ -> QuietHalls_131 <$> liftRunMessage msg attrs
