module Arkham.Types.Location.Cards.OperaGarnier213
  ( operaGarnier213
  , OperaGarnier213(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Action qualified as Action
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target

newtype OperaGarnier213 = OperaGarnier213 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

operaGarnier213 :: LocationCard OperaGarnier213
operaGarnier213 = location
  OperaGarnier213
  Cards.operaGarnier213
  6
  (PerPlayer 1)
  Diamond
  [Triangle, Square, Heart]

instance HasModifiersFor env OperaGarnier213 where
  getModifiersFor (SkillTestSource _ _ _ target (Just Action.Investigate)) (InvestigatorTarget _) (OperaGarnier213 attrs)
    | isTarget attrs target
    = pure $ toModifiers attrs [DoubleBaseSkillValue]
  getModifiersFor _ _ _ = pure []

instance HasAbilities OperaGarnier213 where
  getAbilities (OperaGarnier213 attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env OperaGarnier213 where
  runMessage msg (OperaGarnier213 attrs) =
    OperaGarnier213 <$> runMessage msg attrs
