module Arkham.Location.Cards.OperaGarnier212 (operaGarnier212, OperaGarnier212 (..)) where

import Arkham.GameValue
import Arkham.Helpers.SkillTest (getSkillTest, isInvestigating)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Data.Map.Strict qualified as Map

newtype OperaGarnier212 = OperaGarnier212 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

operaGarnier212 :: LocationCard OperaGarnier212
operaGarnier212 = location OperaGarnier212 Cards.operaGarnier212 5 (PerPlayer 1)

instance HasModifiersFor OperaGarnier212 where
  getModifiersFor (OperaGarnier212 attrs) =
    getSkillTest >>= \case
      Nothing -> pure mempty
      Just st -> do
        investigating <- isInvestigating st.investigator attrs
        if investigating
          then
            Map.unions <$> for (concat $ toList st.committedCards) \card -> do
              modified_ attrs card [DoubleSkillIcons]
          else pure mempty

instance RunMessage OperaGarnier212 where
  runMessage msg (OperaGarnier212 attrs) = OperaGarnier212 <$> runMessage msg attrs
