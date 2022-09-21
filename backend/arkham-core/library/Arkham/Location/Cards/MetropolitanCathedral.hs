module Arkham.Location.Cards.MetropolitanCathedral
  ( metropolitanCathedral
  , MetropolitanCathedral(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype MetropolitanCathedral = MetropolitanCathedral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

metropolitanCathedral :: LocationCard MetropolitanCathedral
metropolitanCathedral =
  location MetropolitanCathedral Cards.metropolitanCathedral 3 (Static 0)

instance HasAbilities MetropolitanCathedral where
  getAbilities (MetropolitanCathedral attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage MetropolitanCathedral where
  runMessage msg (MetropolitanCathedral attrs) =
    MetropolitanCathedral <$> runMessage msg attrs
