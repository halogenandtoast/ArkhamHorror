module Arkham.Asset.Cards.ArcaneInitiate (arcaneInitiate, ArcaneInitiate (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Message qualified as Msg
import Arkham.Matcher
import Arkham.Strategy

newtype ArcaneInitiate = ArcaneInitiate AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneInitiate :: AssetCard ArcaneInitiate
arcaneInitiate = ally ArcaneInitiate Cards.arcaneInitiate (1, 2)

instance HasAbilities ArcaneInitiate where
  getAbilities (ArcaneInitiate a) =
    [ restrictedAbility a 1 ControlsThis $ forced $ AssetEntersPlay #when (be a)
    , restrictedAbility a 2 ControlsThis $ FastAbility (exhaust a)
    ]

instance RunMessage ArcaneInitiate where
  runMessage msg a@(ArcaneInitiate attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 2
      chooseOne
        iid
        [targetLabel iid [Msg.search iid source iid [fromTopOfDeck 3] #spell $ DrawFound iid 1]]
      pure a
    _ -> ArcaneInitiate <$> liftRunMessage msg attrs
