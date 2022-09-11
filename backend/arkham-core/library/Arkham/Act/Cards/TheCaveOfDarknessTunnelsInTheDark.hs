module Arkham.Act.Cards.TheCaveOfDarknessTunnelsInTheDark
  ( TheCaveOfDarknessTunnelsInTheDark(..)
  , theCaveOfDarknessTunnelsInTheDark
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype TheCaveOfDarknessTunnelsInTheDark = TheCaveOfDarknessTunnelsInTheDark ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theCaveOfDarknessTunnelsInTheDark :: ActCard TheCaveOfDarknessTunnelsInTheDark
theCaveOfDarknessTunnelsInTheDark = act
  (2, E)
  TheCaveOfDarknessTunnelsInTheDark
  Cards.theCaveOfDarknessTunnelsInTheDark
  Nothing

instance RunMessage TheCaveOfDarknessTunnelsInTheDark where
  runMessage msg (TheCaveOfDarknessTunnelsInTheDark attrs) =
    TheCaveOfDarknessTunnelsInTheDark <$> runMessage msg attrs
