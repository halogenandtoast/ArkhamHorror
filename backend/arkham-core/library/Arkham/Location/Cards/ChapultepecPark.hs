module Arkham.Location.Cards.ChapultepecPark
  ( chapultepecPark
  , ChapultepecPark(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype ChapultepecPark = ChapultepecPark LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapultepecPark :: LocationCard ChapultepecPark
chapultepecPark = location ChapultepecPark Cards.chapultepecPark 1 (Static 0)

instance HasAbilities ChapultepecPark where
  getAbilities (ChapultepecPark attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage ChapultepecPark where
  runMessage msg (ChapultepecPark attrs) =
    ChapultepecPark <$> runMessage msg attrs
