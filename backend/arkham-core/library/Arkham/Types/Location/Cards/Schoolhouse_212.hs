module Arkham.Types.Location.Cards.Schoolhouse_212
  ( schoolhouse_212
  , Schoolhouse_212(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (schoolhouse_212)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype Schoolhouse_212 = Schoolhouse_212 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

schoolhouse_212 :: LocationCard Schoolhouse_212
schoolhouse_212 = location
  Schoolhouse_212
  Cards.schoolhouse_212
  4
  (Static 1)
  Moon
  [Plus, Squiggle, Circle]

instance HasModifiersFor env Schoolhouse_212 where
  getModifiersFor _ (InvestigatorTarget iid) (Schoolhouse_212 attrs) =
    pure $ toModifiers
      attrs
      [ CannotCommitCards | iid `member` locationInvestigators attrs ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities env Schoolhouse_212 where
  getAbilities = withDrawCardUnderneathAction

instance LocationRunner env => RunMessage env Schoolhouse_212 where
  runMessage msg (Schoolhouse_212 attrs) =
    Schoolhouse_212 <$> runMessage msg attrs
