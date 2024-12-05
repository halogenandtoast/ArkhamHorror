module Arkham.Location.Cards.OperaGarnier213 (operaGarnier213, OperaGarnier213 (..)) where

import Arkham.GameValue
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, isInvestigating)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted

newtype OperaGarnier213 = OperaGarnier213 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

operaGarnier213 :: LocationCard OperaGarnier213
operaGarnier213 = location OperaGarnier213 Cards.operaGarnier213 6 (PerPlayer 1)

instance HasModifiersFor OperaGarnier213 where
  getModifiersFor (OperaGarnier213 attrs) =
    getSkillTestInvestigator >>= \case
      Nothing -> pure mempty
      Just iid -> maybeModified_ attrs iid do
        liftGuardM $ isInvestigating iid attrs.id
        pure [DoubleBaseSkillValue]

instance RunMessage OperaGarnier213 where
  runMessage msg (OperaGarnier213 attrs) = OperaGarnier213 <$> runMessage msg attrs
