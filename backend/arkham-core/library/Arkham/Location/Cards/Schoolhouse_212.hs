module Arkham.Location.Cards.Schoolhouse_212 (
  schoolhouse_212,
  Schoolhouse_212 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (schoolhouse_212)
import Arkham.Location.Helpers
import Arkham.Location.Runner

newtype Schoolhouse_212 = Schoolhouse_212 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

schoolhouse_212 :: LocationCard Schoolhouse_212
schoolhouse_212 = location Schoolhouse_212 Cards.schoolhouse_212 4 (Static 1)

instance HasModifiersFor Schoolhouse_212 where
  getModifiersFor (InvestigatorTarget iid) (Schoolhouse_212 attrs) = do
    here <- iid `isAt` attrs
    pure $ toModifiers attrs [CannotCommitCards #skill | here]
  getModifiersFor _ _ = pure []

instance HasAbilities Schoolhouse_212 where
  getAbilities = withDrawCardUnderneathAction

instance RunMessage Schoolhouse_212 where
  runMessage msg (Schoolhouse_212 attrs) = Schoolhouse_212 <$> runMessage msg attrs
