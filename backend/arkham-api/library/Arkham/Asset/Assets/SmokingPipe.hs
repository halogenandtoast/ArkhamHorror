module Arkham.Asset.Assets.SmokingPipe (smokingPipe) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Investigator
import Arkham.Matcher hiding (FastPlayerWindow)

newtype SmokingPipe = SmokingPipe AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

smokingPipe :: AssetCard SmokingPipe
smokingPipe = asset SmokingPipe Cards.smokingPipe

instance HasAbilities SmokingPipe where
  getAbilities (SmokingPipe a) =
    [ controlled a 1 (exists (HealableInvestigator (toSource a) #horror You))
        $ FastAbility (assetUseCost a Supply 1 <> exhaust a <> DamageCost (toSource a) YouTarget 1)
    ]

instance RunMessage SmokingPipe where
  runMessage msg a@(SmokingPipe attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      whenM (canHaveHorrorHealed (attrs.ability 1) iid) $ healHorror iid (attrs.ability 1) 1
      pure a
    _ -> SmokingPipe <$> liftRunMessage msg attrs
