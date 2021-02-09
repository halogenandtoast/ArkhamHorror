module Arkham.Types.Location.Cards.BishopsBrook_202
  ( bishopsBrook_202
  , BishopsBrook_202(..)
  ) where


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype BishopsBrook_202 = BishopsBrook_202 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bishopsBrook_202 :: BishopsBrook_202
bishopsBrook_202 = BishopsBrook_202 $ baseAttrs
  "02202"
  (Name "Bishop's Brook" Nothing)
  EncounterSet.BloodOnTheAltar
  3
  (Static 2)
  Square
  [Plus, Circle, Triangle]
  [Dunwich]

instance HasModifiersFor env BishopsBrook_202 where
  getModifiersFor _ (EnemyTarget eid) (BishopsBrook_202 attrs@LocationAttrs {..})
    | eid `elem` locationEnemies
    = pure $ toModifiers attrs [HorrorDealt 1]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env BishopsBrook_202 where
  getActions = withDrawCardUnderneathAction

instance LocationRunner env => RunMessage env BishopsBrook_202 where
  runMessage msg (BishopsBrook_202 attrs) =
    BishopsBrook_202 <$> runMessage msg attrs
