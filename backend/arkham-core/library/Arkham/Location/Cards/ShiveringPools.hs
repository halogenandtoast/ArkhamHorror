module Arkham.Location.Cards.ShiveringPools
  ( shiveringPools
  , ShiveringPools(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype ShiveringPools = ShiveringPools LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shiveringPools :: LocationCard ShiveringPools
shiveringPools = location ShiveringPools Cards.shiveringPools 0 (Static 0) NoSymbol []

instance HasAbilities ShiveringPools where
  getAbilities (ShiveringPools attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance LocationRunner env => RunMessage env ShiveringPools where
  runMessage msg (ShiveringPools attrs) =
    ShiveringPools <$> runMessage msg attrs
