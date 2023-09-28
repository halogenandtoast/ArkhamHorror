module Arkham.Location.Cards.VipArea (
  vipArea,
  VipArea (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (vipArea)
import Arkham.Location.Runner
import Arkham.Phase

newtype VipArea = VipArea LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

vipArea :: LocationCard VipArea
vipArea = location VipArea Cards.vipArea 3 (PerPlayer 1)

instance HasModifiersFor VipArea where
  getModifiersFor (InvestigatorTarget iid) (VipArea attrs) = do
    here <- iid `isAt` attrs
    phase <- getPhase
    if here && phase == UpkeepPhase
      then pure $ toModifiers attrs [CannotDrawCards, CannotGainResources]
      else pure []
  getModifiersFor _ _ = pure []

instance RunMessage VipArea where
  runMessage msg (VipArea attrs) = VipArea <$> runMessage msg attrs
