module Arkham.Location.Cards.Schoolhouse_213 (schoolhouse_213, Schoolhouse_213 (..)) where

import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards (schoolhouse_213)
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype Schoolhouse_213 = Schoolhouse_213 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

schoolhouse_213 :: LocationCard Schoolhouse_213
schoolhouse_213 = location Schoolhouse_213 Cards.schoolhouse_213 4 (Static 1)

instance HasModifiersFor Schoolhouse_213 where
  getModifiersFor (InvestigatorTarget _) (Schoolhouse_213 attrs) = do
    pure $ toModifiers attrs [CannotDiscoverCluesExceptAsResultOfInvestigation (be attrs)]
  getModifiersFor _ _ = pure []

instance HasAbilities Schoolhouse_213 where
  getAbilities = withDrawCardUnderneathAction

instance RunMessage Schoolhouse_213 where
  runMessage msg (Schoolhouse_213 attrs) = Schoolhouse_213 <$> runMessage msg attrs
