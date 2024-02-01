module Arkham.Location.Cards.BishopsBrook_202 (
  bishopsBrook_202,
  BishopsBrook_202 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Types (Field (..))
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (bishopsBrook_202)
import Arkham.Location.Runner
import Arkham.Projection

newtype BishopsBrook_202 = BishopsBrook_202 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

bishopsBrook_202 :: LocationCard BishopsBrook_202
bishopsBrook_202 =
  location BishopsBrook_202 Cards.bishopsBrook_202 3 (Static 2)

instance HasModifiersFor BishopsBrook_202 where
  getModifiersFor (EnemyTarget eid) (BishopsBrook_202 attrs) = do
    -- \| eid `elem` locationEnemies =
    atLocation <- fieldMap EnemyLocation (== Just (toId attrs)) eid
    pure $ toModifiers attrs [HorrorDealt 1 | atLocation]
  getModifiersFor _ _ = pure []

instance HasAbilities BishopsBrook_202 where
  getAbilities = withDrawCardUnderneathAction

instance RunMessage BishopsBrook_202 where
  runMessage msg (BishopsBrook_202 attrs) =
    BishopsBrook_202 <$> runMessage msg attrs
