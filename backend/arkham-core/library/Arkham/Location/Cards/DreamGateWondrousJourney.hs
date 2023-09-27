module Arkham.Location.Cards.DreamGateWondrousJourney (
  dreamGateWondrousJourney,
  DreamGateWondrousJourney (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype DreamGateWondrousJourney = DreamGateWondrousJourney LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamGateWondrousJourney :: LocationCard DreamGateWondrousJourney
dreamGateWondrousJourney =
  locationWith DreamGateWondrousJourney Cards.dreamGateWondrousJourney 1 (Static 0)
    $ (connectedMatchersL .~ [Anywhere])
    . (revealedConnectedMatchersL .~ [Anywhere])

instance HasModifiersFor DreamGateWondrousJourney where
  getModifiersFor (LocationTarget lid) (DreamGateWondrousJourney a) = do
    pure $ toModifiers a [ConnectedToWhen (LocationWithId lid) (LocationWithId $ toId a)]
  getModifiersFor _ _ = pure []

instance HasAbilities DreamGateWondrousJourney where
  getAbilities (DreamGateWondrousJourney attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage DreamGateWondrousJourney where
  runMessage msg (DreamGateWondrousJourney attrs) =
    DreamGateWondrousJourney <$> runMessage msg attrs
