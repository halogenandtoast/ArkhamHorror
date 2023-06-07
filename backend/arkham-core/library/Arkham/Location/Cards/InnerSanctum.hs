module Arkham.Location.Cards.InnerSanctum
  ( innerSanctum
  , InnerSanctum(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype InnerSanctum = InnerSanctum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

innerSanctum :: LocationCard InnerSanctum
innerSanctum = location InnerSanctum Cards.innerSanctum 4 (PerPlayer 1)

instance HasAbilities InnerSanctum where
  getAbilities (InnerSanctum attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage InnerSanctum where
  runMessage msg (InnerSanctum attrs) =
    InnerSanctum <$> runMessage msg attrs
