module Arkham.Asset.Cards.ArcaneInitiate3 (arcaneInitiate3, ArcaneInitiate3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Message qualified as Msg
import Arkham.Matcher
import Arkham.Strategy

newtype ArcaneInitiate3 = ArcaneInitiate3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneInitiate3 :: AssetCard ArcaneInitiate3
arcaneInitiate3 = ally ArcaneInitiate3 Cards.arcaneInitiate3 (1, 3)

instance HasAbilities ArcaneInitiate3 where
  getAbilities (ArcaneInitiate3 a) =
    [ restrictedAbility a 1 ControlsThis $ forced $ AssetEntersPlay #when (be a)
    , restrictedAbility a 2 ControlsThis $ FastAbility (exhaust a)
    ]

instance RunMessage ArcaneInitiate3 where
  runMessage msg a@(ArcaneInitiate3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOne
        iid
        [ Label "Place 1 doom" [Msg.placeDoom (attrs.ability 1) attrs 1]
        , Label "Place 2 horror" [Msg.placeHorror (attrs.ability 1) attrs 2]
        ]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 2
      chooseOne
        iid
        [targetLabel iid [Msg.search iid source iid [fromTopOfDeck 3] #spell $ DrawFound iid 1]]
      pure a
    _ -> ArcaneInitiate3 <$> runMessage msg attrs
