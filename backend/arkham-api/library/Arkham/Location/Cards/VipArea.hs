module Arkham.Location.Cards.VipArea (vipArea, VipArea (..)) where

import Arkham.Classes
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (vipArea)
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Phase
import Arkham.Prelude

newtype VipArea = VipArea LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

vipArea :: LocationCard VipArea
vipArea = location VipArea Cards.vipArea 3 (PerPlayer 1)

instance HasModifiersFor VipArea where
  getModifiersFor (VipArea a) = do
    phase <- getPhase
    modifySelectWhen a (phase == UpkeepPhase) (investigatorAt a) [CannotDrawCards, CannotGainResources]

instance RunMessage VipArea where
  runMessage msg (VipArea attrs) = VipArea <$> runMessage msg attrs
