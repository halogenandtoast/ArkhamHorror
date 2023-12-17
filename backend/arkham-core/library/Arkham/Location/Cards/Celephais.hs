module Arkham.Location.Cards.Celephais
  ( celephais
  , Celephais(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Celephais = Celephais LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

celephais :: LocationCard Celephais
celephais = location Celephais Cards.celephais 2 (PerPlayer 1)

instance HasAbilities Celephais where
  getAbilities (Celephais attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage Celephais where
  runMessage msg (Celephais attrs) =
    Celephais <$> runMessage msg attrs
