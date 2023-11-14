module Arkham.Location.Cards.TheEnchantedPath
  ( theEnchantedPath
  , TheEnchantedPath(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype TheEnchantedPath = TheEnchantedPath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEnchantedPath :: LocationCard TheEnchantedPath
theEnchantedPath = location TheEnchantedPath Cards.theEnchantedPath 2 (Static 0)

instance HasAbilities TheEnchantedPath where
  getAbilities (TheEnchantedPath attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage TheEnchantedPath where
  runMessage msg (TheEnchantedPath attrs) =
    TheEnchantedPath <$> runMessage msg attrs
