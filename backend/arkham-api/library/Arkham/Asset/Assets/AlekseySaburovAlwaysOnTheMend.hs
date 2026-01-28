module Arkham.Asset.Assets.AlekseySaburovAlwaysOnTheMend (alekseySaburovAlwaysOnTheMend) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Healing
import Arkham.Matcher

newtype AlekseySaburovAlwaysOnTheMend = AlekseySaburovAlwaysOnTheMend AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alekseySaburovAlwaysOnTheMend :: AssetCard AlekseySaburovAlwaysOnTheMend
alekseySaburovAlwaysOnTheMend = ally AlekseySaburovAlwaysOnTheMend Cards.alekseySaburovAlwaysOnTheMend (2, 2)

instance HasAbilities AlekseySaburovAlwaysOnTheMend where
  getAbilities (AlekseySaburovAlwaysOnTheMend x) =
    [ controlled x 1 (exists $ healableAsset (x.ability 1) (be x))
        $ freeReaction
        $ TurnBegins #when You
    ]

instance RunMessage AlekseySaburovAlwaysOnTheMend where
  runMessage msg a@(AlekseySaburovAlwaysOnTheMend attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assetChooseHealDamageOrHorror (attrs.ability 1) iid attrs.id
      pure a
    _ -> AlekseySaburovAlwaysOnTheMend <$> liftRunMessage msg attrs
