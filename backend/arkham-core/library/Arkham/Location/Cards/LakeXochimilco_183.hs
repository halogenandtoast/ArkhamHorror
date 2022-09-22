module Arkham.Location.Cards.LakeXochimilco_183
  ( lakeXochimilco_183
  , LakeXochimilco_183(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype LakeXochimilco_183 = LakeXochimilco_183 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lakeXochimilco_183 :: LocationCard LakeXochimilco_183
lakeXochimilco_183 = locationWith
  LakeXochimilco_183
  Cards.lakeXochimilco_183
  4
  (PerPlayer 2)
  (labelL .~ "heart")

instance HasAbilities LakeXochimilco_183 where
  getAbilities (LakeXochimilco_183 attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage LakeXochimilco_183 where
  runMessage msg (LakeXochimilco_183 attrs) =
    LakeXochimilco_183 <$> runMessage msg attrs
