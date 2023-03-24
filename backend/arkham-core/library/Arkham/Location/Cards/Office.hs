module Arkham.Location.Cards.Office
  ( office
  , Office(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype Office = Office LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

office :: LocationCard Office
office = location Office Cards.office 4 (PerPlayer 1)

instance HasAbilities Office where
  getAbilities (Office attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage Office where
  runMessage msg (Office attrs) =
    Office <$> runMessage msg attrs
