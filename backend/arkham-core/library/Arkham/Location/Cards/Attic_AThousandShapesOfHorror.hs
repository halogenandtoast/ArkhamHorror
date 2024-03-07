module Arkham.Location.Cards.Attic_AThousandShapesOfHorror
  ( attic_AThousandShapesOfHorror
  , Attic_AThousandShapesOfHorror(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Attic_AThousandShapesOfHorror = Attic_AThousandShapesOfHorror LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

attic_AThousandShapesOfHorror :: LocationCard Attic_AThousandShapesOfHorror
attic_AThousandShapesOfHorror = location Attic_AThousandShapesOfHorror Cards.attic_AThousandShapesOfHorror 0 (Static 0)

instance HasAbilities Attic_AThousandShapesOfHorror where
  getAbilities (Attic_AThousandShapesOfHorror attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage Attic_AThousandShapesOfHorror where
  runMessage msg (Attic_AThousandShapesOfHorror attrs) =
    Attic_AThousandShapesOfHorror <$> runMessage msg attrs
