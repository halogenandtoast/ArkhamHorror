module Arkham.Location.Cards.DylathLeen
  ( dylathLeen
  , DylathLeen(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype DylathLeen = DylathLeen LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dylathLeen :: LocationCard DylathLeen
dylathLeen = location DylathLeen Cards.dylathLeen 3 (PerPlayer 1)

instance HasAbilities DylathLeen where
  getAbilities (DylathLeen attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage DylathLeen where
  runMessage msg (DylathLeen attrs) =
    DylathLeen <$> runMessage msg attrs
