module Arkham.Location.Cards.QuietHalls_135 (quietHalls_135) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait

newtype QuietHalls_135 = QuietHalls_135 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quietHalls_135 :: LocationCard QuietHalls_135
quietHalls_135 =
  locationWith QuietHalls_135 Cards.quietHalls_135 3 (Static 0)
    $ (connectedMatchersL <>~ [LocationWithTrait ThirdFloor])
    . (revealedConnectedMatchersL <>~ [LocationWithTrait ThirdFloor])

instance HasAbilities QuietHalls_135 where
  getAbilities (QuietHalls_135 a) =
    extendRevealed1 a
      $ restricted
        a
        1
        (Here <> notExists UnrevealedLocation <> notExists LocationWithAnyClues)
        actionAbility

instance RunMessage QuietHalls_135 where
  runMessage msg l@(QuietHalls_135 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeClues (attrs.ability 1) attrs =<< getPlayerCount
      pure l
    _ -> QuietHalls_135 <$> liftRunMessage msg attrs
