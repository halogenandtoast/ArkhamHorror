module Arkham.Asset.Assets.WalterFitzpatrickPlayingBothSides2 (walterFitzpatrickPlayingBothSides2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (isParley)
import Arkham.Matcher

newtype WalterFitzpatrickPlayingBothSides2 = WalterFitzpatrickPlayingBothSides2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

walterFitzpatrickPlayingBothSides2 :: AssetCard WalterFitzpatrickPlayingBothSides2
walterFitzpatrickPlayingBothSides2 = ally WalterFitzpatrickPlayingBothSides2 Cards.walterFitzpatrickPlayingBothSides2 (2, 2)

instance HasModifiersFor WalterFitzpatrickPlayingBothSides2 where
  getModifiersFor (WalterFitzpatrickPlayingBothSides2 attrs) = controllerGetsMaybe attrs \_ -> do
    liftGuardM isParley
    pure [AnySkillValue 1]

instance HasAbilities WalterFitzpatrickPlayingBothSides2 where
  getAbilities (WalterFitzpatrickPlayingBothSides2 a) =
    [ controlled a 1 (youExist can.gain.resources)
        $ triggered (SuccessfulParley #after You) (exhaust a)
    ]

instance RunMessage WalterFitzpatrickPlayingBothSides2 where
  runMessage msg a@(WalterFitzpatrickPlayingBothSides2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResources iid (attrs.ability 1) 1
      pure a
    _ -> WalterFitzpatrickPlayingBothSides2 <$> liftRunMessage msg attrs
