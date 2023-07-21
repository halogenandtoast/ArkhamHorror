module Arkham.Location.Cards.GardensOfLuxembourg (
  gardensOfLuxembourg,
  GardensOfLuxembourg (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Trait

newtype GardensOfLuxembourg = GardensOfLuxembourg LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gardensOfLuxembourg :: LocationCard GardensOfLuxembourg
gardensOfLuxembourg =
  location GardensOfLuxembourg Cards.gardensOfLuxembourg 3 (PerPlayer 1)

instance HasAbilities GardensOfLuxembourg where
  getAbilities (GardensOfLuxembourg attrs) = getAbilities attrs

instance HasModifiersFor GardensOfLuxembourg where
  getModifiersFor (LocationTarget lid) (GardensOfLuxembourg attrs)
    | toId attrs == lid && locationRevealed attrs = do
        byakheeIsMoving <-
          selectAny
            (MovingEnemy <> EnemyWithTrait Byakhee <> EnemyAt (LocationWithId lid))
        pure $
          toModifiers
            attrs
            [ ConnectedToWhen (LocationWithId lid) (LocationWithId $ toId attrs)
            | byakheeIsMoving
            ]
  getModifiersFor _ _ = pure []

instance RunMessage GardensOfLuxembourg where
  runMessage msg (GardensOfLuxembourg attrs) =
    GardensOfLuxembourg <$> runMessage msg attrs
