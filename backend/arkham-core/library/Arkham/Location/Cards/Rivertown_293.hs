module Arkham.Location.Cards.Rivertown_293
  ( rivertown_293
  , Rivertown_293(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Rivertown_293 = Rivertown_293 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rivertown_293 :: LocationCard Rivertown_293
rivertown_293 = location Rivertown_293 Cards.rivertown_293 4 (Static 0)

instance HasAbilities Rivertown_293 where
  getAbilities (Rivertown_293 attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage Rivertown_293 where
  runMessage msg (Rivertown_293 attrs) =
    Rivertown_293 <$> runMessage msg attrs
