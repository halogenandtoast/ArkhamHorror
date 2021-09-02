module Arkham.Types.Location.Cards.BishopsBrook_202
  ( bishopsBrook_202
  , BishopsBrook_202(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (bishopsBrook_202)
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype BishopsBrook_202 = BishopsBrook_202 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bishopsBrook_202 :: LocationCard BishopsBrook_202
bishopsBrook_202 = location
  BishopsBrook_202
  Cards.bishopsBrook_202
  3
  (Static 2)
  Square
  [Plus, Circle, Triangle]

instance HasModifiersFor env BishopsBrook_202 where
  getModifiersFor _ (EnemyTarget eid) (BishopsBrook_202 attrs@LocationAttrs {..})
    | eid `elem` locationEnemies
    = pure $ toModifiers attrs [HorrorDealt 1]
  getModifiersFor _ _ _ = pure []

instance HasAbilities BishopsBrook_202 where
  getAbilities = withDrawCardUnderneathAction

instance LocationRunner env => RunMessage env BishopsBrook_202 where
  runMessage msg (BishopsBrook_202 attrs) =
    BishopsBrook_202 <$> runMessage msg attrs
