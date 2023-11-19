module Arkham.Location.Cards.TheEnchantedPath (
  theEnchantedPath,
  TheEnchantedPath (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype TheEnchantedPath = TheEnchantedPath LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEnchantedPath :: LocationCard TheEnchantedPath
theEnchantedPath = location TheEnchantedPath Cards.theEnchantedPath 2 (Static 0)

instance HasModifiersFor TheEnchantedPath where
  getModifiersFor target (TheEnchantedPath a) | a `is` target = do
    blocked <- selectAny $ locationIs Cards.baseOfTheSteps <> LocationWithAnyClues
    pure $ toModifiers a [Blocked | blocked]
  getModifiersFor _ _ = pure []

instance HasAbilities TheEnchantedPath where
  getAbilities (TheEnchantedPath attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage TheEnchantedPath where
  runMessage msg (TheEnchantedPath attrs) =
    TheEnchantedPath <$> runMessage msg attrs
