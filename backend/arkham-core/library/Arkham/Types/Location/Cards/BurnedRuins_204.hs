module Arkham.Types.Location.Cards.BurnedRuins_204
  ( burnedRuins_204
  , BurnedRuins_204(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (burnedRuins_204)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype BurnedRuins_204 = BurnedRuins_204 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burnedRuins_204 :: LocationId -> BurnedRuins_204
burnedRuins_204 = BurnedRuins_204 . baseAttrs
  Cards.burnedRuins_204
  3
  (Static 3)
  Triangle
  [Square, Diamond]

instance HasModifiersFor env BurnedRuins_204 where
  getModifiersFor _ (EnemyTarget eid) (BurnedRuins_204 attrs@LocationAttrs {..})
    | eid `elem` locationEnemies = pure $ toModifiers attrs [EnemyEvade 1]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env BurnedRuins_204 where
  getActions = withDrawCardUnderneathAction

instance LocationRunner env => RunMessage env BurnedRuins_204 where
  runMessage msg (BurnedRuins_204 attrs) =
    BurnedRuins_204 <$> runMessage msg attrs
