module Arkham.Types.Location.Cards.Schoolhouse_212
  ( schoolhouse_212
  , Schoolhouse_212(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (schoolhouse_212)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype Schoolhouse_212 = Schoolhouse_212 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

schoolhouse_212 :: LocationId -> Schoolhouse_212
schoolhouse_212 = Schoolhouse_212 . baseAttrs
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

instance ActionRunner env => HasActions env Schoolhouse_212 where
  getActions = withDrawCardUnderneathAction

instance LocationRunner env => RunMessage env Schoolhouse_212 where
  runMessage msg (Schoolhouse_212 attrs) =
    Schoolhouse_212 <$> runMessage msg attrs
