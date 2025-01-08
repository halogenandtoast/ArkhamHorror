module Arkham.Location.Cards.BishopsBrook_202 (bishopsBrook_202, BishopsBrook_202 (..)) where

import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (bishopsBrook_202)
import Arkham.Location.Import.Lifted
import Arkham.Location.Runner (withDrawCardUnderneathAction)
import Arkham.Matcher

newtype BishopsBrook_202 = BishopsBrook_202 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bishopsBrook_202 :: LocationCard BishopsBrook_202
bishopsBrook_202 = location BishopsBrook_202 Cards.bishopsBrook_202 3 (Static 2)

instance HasModifiersFor BishopsBrook_202 where
  getModifiersFor (BishopsBrook_202 a) =
    modifySelect a (AttackingEnemy <> enemyAt a.id) [HorrorDealt 1]

instance HasAbilities BishopsBrook_202 where
  getAbilities = withDrawCardUnderneathAction

instance RunMessage BishopsBrook_202 where
  runMessage msg (BishopsBrook_202 attrs) = BishopsBrook_202 <$> runMessage msg attrs
