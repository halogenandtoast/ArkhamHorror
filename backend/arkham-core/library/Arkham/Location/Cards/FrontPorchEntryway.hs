module Arkham.Location.Cards.FrontPorchEntryway
  ( frontPorchEntryway
  , FrontPorchEntryway(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype FrontPorchEntryway = FrontPorchEntryway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frontPorchEntryway :: LocationCard FrontPorchEntryway
frontPorchEntryway = location FrontPorchEntryway Cards.frontPorchEntryway 0 (Static 0)

instance HasAbilities FrontPorchEntryway where
  getAbilities (FrontPorchEntryway attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage FrontPorchEntryway where
  runMessage msg (FrontPorchEntryway attrs) =
    FrontPorchEntryway <$> runMessage msg attrs
