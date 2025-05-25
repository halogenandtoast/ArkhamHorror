module Arkham.Asset.Assets.KeyOfYs (keyOfYs) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype KeyOfYs = KeyOfYs AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keyOfYs :: AssetCard KeyOfYs
keyOfYs = assetWith KeyOfYs Cards.keyOfYs (sanityL ?~ 4)

instance HasModifiersFor KeyOfYs where
  getModifiersFor (KeyOfYs a) = controllerGets a [AnySkillValue a.horror]

instance HasAbilities KeyOfYs where
  getAbilities (KeyOfYs x) =
    [ restricted x 1 ControlsThis $ forced $ PlacedCounter #when You AnySource #horror (atLeast 1)
    , restricted x 2 ControlsThis $ forced $ AssetLeavesPlay #when (be x)
    ]

instance RunMessage KeyOfYs where
  runMessage msg a@(KeyOfYs attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ ReassignHorror (toSource iid) (toTarget attrs) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      discardTopOfDeck iid (attrs.ability 2) 10
      pure a
    _ -> KeyOfYs <$> liftRunMessage msg attrs
