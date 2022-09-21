module Arkham.Location.Cards.LakeXochimilco_182
  ( lakeXochimilco_182
  , LakeXochimilco_182(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype LakeXochimilco_182 = LakeXochimilco_182 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lakeXochimilco_182 :: LocationCard LakeXochimilco_182
lakeXochimilco_182 = location LakeXochimilco_182 Cards.lakeXochimilco_182 2 (PerPlayer 1)

instance HasAbilities LakeXochimilco_182 where
  getAbilities (LakeXochimilco_182 attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage LakeXochimilco_182 where
  runMessage msg (LakeXochimilco_182 attrs) =
    LakeXochimilco_182 <$> runMessage msg attrs
