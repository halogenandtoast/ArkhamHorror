module Arkham.Types.Location.Cards.Schoolhouse_213
  ( schoolhouse_213
  , Schoolhouse_213(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (schoolhouse_213)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message

newtype Schoolhouse_213 = Schoolhouse_213 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

schoolhouse_213 :: LocationCard Schoolhouse_213
schoolhouse_213 = location
  Schoolhouse_213
  Cards.schoolhouse_213
  4
  (Static 1)
  Moon
  [Plus, Squiggle, Circle]

instance HasModifiersFor env Schoolhouse_213


instance ActionRunner env => HasAbilities env Schoolhouse_213 where
  getAbilities = withDrawCardUnderneathAction

instance LocationRunner env => RunMessage env Schoolhouse_213 where
  runMessage msg l@(Schoolhouse_213 attrs) = case msg of
    -- Cannot discover clues except by investigating so we just noop
    DiscoverCluesAtLocation _ lid _ Nothing | lid == locationId attrs -> pure l
    _ -> Schoolhouse_213 <$> runMessage msg attrs
