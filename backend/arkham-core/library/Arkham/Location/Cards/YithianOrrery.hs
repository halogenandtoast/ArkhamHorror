module Arkham.Location.Cards.YithianOrrery
  ( yithianOrrery
  , YithianOrrery(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype YithianOrrery = YithianOrrery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yithianOrrery :: LocationCard YithianOrrery
yithianOrrery = location YithianOrrery Cards.yithianOrrery 4 (PerPlayer 1)

instance HasAbilities YithianOrrery where
  getAbilities (YithianOrrery attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage YithianOrrery where
  runMessage msg (YithianOrrery attrs) = YithianOrrery <$> runMessage msg attrs
