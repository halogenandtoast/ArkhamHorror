module Arkham.Location.Cards.HiddenTunnelAWayOut (hiddenTunnelAWayOut) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype HiddenTunnelAWayOut = HiddenTunnelAWayOut LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiddenTunnelAWayOut :: LocationCard HiddenTunnelAWayOut
hiddenTunnelAWayOut = location HiddenTunnelAWayOut Cards.hiddenTunnelAWayOut 5 (Static 0)

instance HasAbilities HiddenTunnelAWayOut where
  getAbilities (HiddenTunnelAWayOut a) =
    extendRevealed
      a
      [restricted a 1 (EachUndefeatedInvestigator $ at_ (be a)) $ Objective $ forced AnyWindow]

instance RunMessage HiddenTunnelAWayOut where
  runMessage msg l@(HiddenTunnelAWayOut attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther =<< selectJust AnyAct
      pure l
    _ -> HiddenTunnelAWayOut <$> liftRunMessage msg attrs
