module Arkham.Location.Cards.PlateauOfLeng
  ( plateauOfLeng
  , PlateauOfLeng(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype PlateauOfLeng = PlateauOfLeng LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

plateauOfLeng :: LocationCard PlateauOfLeng
plateauOfLeng = location PlateauOfLeng Cards.plateauOfLeng 3 (Static 1)

instance HasAbilities PlateauOfLeng where
  getAbilities (PlateauOfLeng attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage PlateauOfLeng where
  runMessage msg (PlateauOfLeng attrs) =
    PlateauOfLeng <$> runMessage msg attrs
