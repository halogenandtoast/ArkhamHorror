module Arkham.Location.Cards.Schoolhouse_213 (schoolhouse_213, Schoolhouse_213 (..)) where

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards (schoolhouse_213)
import Arkham.Location.Import.Lifted
import Arkham.Location.Runner (withDrawCardUnderneathAction)
import Arkham.Matcher

newtype Schoolhouse_213 = Schoolhouse_213 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

schoolhouse_213 :: LocationCard Schoolhouse_213
schoolhouse_213 = location Schoolhouse_213 Cards.schoolhouse_213 4 (Static 1)

instance HasModifiersFor Schoolhouse_213 where
  getModifiersFor (Schoolhouse_213 a) =
    whenRevealed a $ modifySelect a Anyone [CannotDiscoverCluesExceptAsResultOfInvestigation (be a)]

instance HasAbilities Schoolhouse_213 where
  getAbilities = withDrawCardUnderneathAction

instance RunMessage Schoolhouse_213 where
  runMessage msg (Schoolhouse_213 attrs) = Schoolhouse_213 <$> runMessage msg attrs
