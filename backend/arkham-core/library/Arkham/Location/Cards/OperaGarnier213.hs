module Arkham.Location.Cards.OperaGarnier213
  ( operaGarnier213
  , OperaGarnier213(..)
  ) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.SkillTest
import Arkham.Source
import Arkham.Target

newtype OperaGarnier213 = OperaGarnier213 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

operaGarnier213 :: LocationCard OperaGarnier213
operaGarnier213 =
  location OperaGarnier213 Cards.operaGarnier213 6 (PerPlayer 1)

instance HasModifiersFor OperaGarnier213 where
  getModifiersFor (SkillTestSource _ _ _ (Just Action.Investigate)) (InvestigatorTarget _) (OperaGarnier213 attrs)
    = do
      mtarget <- getSkillTestTarget
      case mtarget of
        Just target | isTarget attrs target ->
          pure $ toModifiers attrs [DoubleBaseSkillValue]
        _ -> pure []
  getModifiersFor _ _ _ = pure []

instance HasAbilities OperaGarnier213 where
  getAbilities (OperaGarnier213 attrs) = getAbilities attrs

instance RunMessage OperaGarnier213 where
  runMessage msg (OperaGarnier213 attrs) =
    OperaGarnier213 <$> runMessage msg attrs
