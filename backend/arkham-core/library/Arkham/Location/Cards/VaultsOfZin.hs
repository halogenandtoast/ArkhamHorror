module Arkham.Location.Cards.VaultsOfZin
  ( vaultsOfZin
  , VaultsOfZin(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype VaultsOfZin = VaultsOfZin LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vaultsOfZin :: LocationCard VaultsOfZin
vaultsOfZin = location VaultsOfZin Cards.vaultsOfZin 3 (PerPlayer 1)

instance HasAbilities VaultsOfZin where
  getAbilities (VaultsOfZin attrs) =
    extendRevealed attrs []

instance RunMessage VaultsOfZin where
  runMessage msg (VaultsOfZin attrs) =
    VaultsOfZin <$> runMessage msg attrs
