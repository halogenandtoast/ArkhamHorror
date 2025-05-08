module Arkham.Asset.Assets.SpareParts (spareParts) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype SpareParts = SpareParts AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spareParts :: AssetCard SpareParts
spareParts = asset SpareParts Cards.spareParts

instance HasAbilities SpareParts where
  getAbilities (SpareParts a) =
    [ storyControlled
        a
        1
        ( oneOf
            [ exists $ AssetAt YourLocation <> AssetCanHaveUses Supply
            , exists $ colocatedWithMatch You <> can.gain.resources
            ]
        )
        $ FastAbility (exhaust a <> assetUseCost a Supply 1)
    ]

instance RunMessage SpareParts where
  runMessage msg a@(SpareParts attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ AssetAt (locationWithInvestigator iid) <> AssetCanHaveUses Supply
      investigators <- select $ colocatedWith iid <> can.gain.resources

      chooseOneM iid do
        targets assets \x -> push $ AddUses (attrs.ability 1) x Supply 1
        targets investigators \x -> gainResourcesIfCan x (attrs.ability 1) 1

      pure a
    _ -> SpareParts <$> liftRunMessage msg attrs
