module Arkham.Location.Cards.VipArea (vipArea) where

import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards (vipArea)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Phase

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
