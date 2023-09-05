module Arkham.Location.Cards.Rivertown_292
  ( rivertown_292
  , Rivertown_292(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Rivertown_292 = Rivertown_292 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rivertown_292 :: LocationCard Rivertown_292
rivertown_292 = location Rivertown_292 Cards.rivertown_292 3 (Static 0)

instance HasAbilities Rivertown_292 where
  getAbilities (Rivertown_292 attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage Rivertown_292 where
  runMessage msg (Rivertown_292 attrs) =
    Rivertown_292 <$> runMessage msg attrs
