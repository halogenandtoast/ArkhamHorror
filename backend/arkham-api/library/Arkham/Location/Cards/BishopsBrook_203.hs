module Arkham.Location.Cards.BishopsBrook_203 (bishopsBrook_203, BishopsBrook_203 (..)) where

import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (bishopsBrook_203)
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype BishopsBrook_203 = BishopsBrook_203 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bishopsBrook_203 :: LocationCard BishopsBrook_203
bishopsBrook_203 = location BishopsBrook_203 Cards.bishopsBrook_203 3 (Static 2)

instance HasModifiersFor BishopsBrook_203 where
  getModifiersFor (BishopsBrook_203 attrs) = do
    anyHere <- selectAny $ investigatorAt attrs
    modifySelectWhen attrs anyHere (not_ $ investigatorAt attrs) [CannotEnter (toId attrs)]

instance HasAbilities BishopsBrook_203 where
  getAbilities = withDrawCardUnderneathAction

instance RunMessage BishopsBrook_203 where
  runMessage msg (BishopsBrook_203 attrs) = BishopsBrook_203 <$> runMessage msg attrs
