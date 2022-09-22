module Arkham.Location.Cards.Xochimilco
  ( xochimilco
  , Xochimilco(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Xochimilco = Xochimilco LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

xochimilco :: LocationCard Xochimilco
xochimilco =
  locationWith Xochimilco Cards.xochimilco 4 (Static 0) (labelL .~ "heart")

instance HasAbilities Xochimilco where
  getAbilities (Xochimilco attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage Xochimilco where
  runMessage msg (Xochimilco attrs) = Xochimilco <$> runMessage msg attrs
