module Arkham.Location.Cards.ExpeditionCamp
  ( expeditionCamp
  , ExpeditionCamp(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype ExpeditionCamp = ExpeditionCamp LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

expeditionCamp :: LocationCard ExpeditionCamp
expeditionCamp = location ExpeditionCamp Cards.expeditionCamp 0 (Static 0) NoSymbol []

instance HasAbilities ExpeditionCamp where
  getAbilities (ExpeditionCamp attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage ExpeditionCamp where
  runMessage msg (ExpeditionCamp attrs) =
    ExpeditionCamp <$> runMessage msg attrs
