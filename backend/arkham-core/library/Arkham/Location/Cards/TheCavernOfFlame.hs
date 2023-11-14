module Arkham.Location.Cards.TheCavernOfFlame
  ( theCavernOfFlame
  , TheCavernOfFlame(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype TheCavernOfFlame = TheCavernOfFlame LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCavernOfFlame :: LocationCard TheCavernOfFlame
theCavernOfFlame = location TheCavernOfFlame Cards.theCavernOfFlame 9 (Static 0)

instance HasAbilities TheCavernOfFlame where
  getAbilities (TheCavernOfFlame attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage TheCavernOfFlame where
  runMessage msg (TheCavernOfFlame attrs) =
    TheCavernOfFlame <$> runMessage msg attrs
