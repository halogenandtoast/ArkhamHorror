module Arkham.Location.Cards.Baharna
  ( baharna
  , Baharna(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Baharna = Baharna LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baharna :: LocationCard Baharna
baharna = location Baharna Cards.baharna 2 (PerPlayer 1)

instance HasAbilities Baharna where
  getAbilities (Baharna attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage Baharna where
  runMessage msg (Baharna attrs) =
    Baharna <$> runMessage msg attrs
