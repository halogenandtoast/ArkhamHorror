module Arkham.Types.Location.Cards.Infirmary
  ( infirmary
  , Infirmary(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype Infirmary = Infirmary LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

infirmary :: LocationCard Infirmary
infirmary =
  location Infirmary Cards.infirmary 3 (PerPlayer 1) Heart [Hourglass]

instance HasAbilities Infirmary where
  getAbilities (Infirmary attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env Infirmary where
  runMessage msg (Infirmary attrs) = Infirmary <$> runMessage msg attrs
