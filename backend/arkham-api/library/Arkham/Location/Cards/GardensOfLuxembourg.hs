module Arkham.Location.Cards.GardensOfLuxembourg (gardensOfLuxembourg, GardensOfLuxembourg (..)) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait

newtype GardensOfLuxembourg = GardensOfLuxembourg LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

gardensOfLuxembourg :: LocationCard GardensOfLuxembourg
gardensOfLuxembourg = location GardensOfLuxembourg Cards.gardensOfLuxembourg 3 (PerPlayer 1)

instance HasModifiersFor GardensOfLuxembourg where
  getModifiersFor (LocationTarget lid) (GardensOfLuxembourg attrs) | attrs.id == lid && attrs.revealed = do
    pure
      $ toModifiers
        attrs
        [ ConnectedToWhen
            (LocationWithEnemy $ MovingEnemy <> EnemyWithTrait Byakhee)
            (LocationWithId $ toId attrs)
        ]
  getModifiersFor _ _ = pure []

instance RunMessage GardensOfLuxembourg where
  runMessage msg (GardensOfLuxembourg attrs) =
    GardensOfLuxembourg <$> runMessage msg attrs
