module Arkham.Location.Cards.Vault
  ( vault
  , Vault(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Vault = Vault LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vault :: LocationCard Vault
vault = location Vault Cards.vault 4 (PerPlayer 1)

instance HasAbilities Vault where
  getAbilities (Vault attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage Vault where
  runMessage msg (Vault attrs) =
    Vault <$> runMessage msg attrs
