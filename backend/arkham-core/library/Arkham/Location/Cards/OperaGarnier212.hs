module Arkham.Location.Cards.OperaGarnier212
  ( operaGarnier212
  , OperaGarnier212(..)
  ) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.GameValue
import Arkham.Modifier
import qualified Arkham.Location.Cards as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Source
import Arkham.Target

newtype OperaGarnier212 = OperaGarnier212 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

operaGarnier212 :: LocationCard OperaGarnier212
operaGarnier212 = location
  OperaGarnier212
  Cards.operaGarnier212
  5
  (PerPlayer 1)
  Diamond
  [Triangle, Square, Heart]

instance HasModifiersFor env OperaGarnier212 where
  getModifiersFor (SkillTestSource _ _ _ target (Just Action.Investigate)) (CardIdTarget _) (OperaGarnier212 attrs) | isTarget attrs target =
    pure $ toModifiers attrs [DoubleSkillIcons]

instance HasAbilities OperaGarnier212 where
  getAbilities (OperaGarnier212 attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env OperaGarnier212 where
  runMessage msg (OperaGarnier212 attrs) =
    OperaGarnier212 <$> runMessage msg attrs
