module Arkham.Asset.Assets.OliveMcBride2 (oliveMcBride2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosBagStepState
import Arkham.Helpers.Window
import Arkham.Matcher

newtype OliveMcBride2 = OliveMcBride2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oliveMcBride2 :: AssetCard OliveMcBride2
oliveMcBride2 = ally OliveMcBride2 Cards.oliveMcBride2 (1, 3)

instance HasAbilities OliveMcBride2 where
  getAbilities (OliveMcBride2 a) =
    [restricted a 1 ControlsThis $ triggered (WouldRevealChaosToken #when You) (exhaust a)]

instance RunMessage OliveMcBride2 where
  runMessage msg a@(OliveMcBride2 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getDrawSource -> drawSource) _ -> do
      push
        $ ReplaceCurrentDraw drawSource iid
        $ Choose
          (toSource attrs)
          2
          ResolveChoice
          [Undecided Draw, Undecided Draw, Undecided Draw, Undecided Draw]
          []
          Nothing
      cancelledOrIgnoredCardOrGameEffect (attrs.ability 1)
      pure a
    _ -> OliveMcBride2 <$> liftRunMessage msg attrs
