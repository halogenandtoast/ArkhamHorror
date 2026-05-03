module Arkham.Asset.Assets.ExtraRations1 (extraRations1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ExtraRations1 = ExtraRations1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

extraRations1 :: AssetCard ExtraRations1
extraRations1 = assetWith ExtraRations1 Cards.extraRations1 discardWhenNoUses

instance HasAbilities ExtraRations1 where
  getAbilities (ExtraRations1 a) =
    [ controlled
        a
        1
        ( oneOf
            [ exists $ HealableInvestigator (a.ability 1) #damage You
            , exists $ HealableAsset (a.ability 1) #damage $ #ally <> AssetControlledBy You
            ]
        )
        $ freeTrigger (assetUseCost a Supply 1 <> exhaust a)
    ]

instance RunMessage ExtraRations1 where
  runMessage msg a@(ExtraRations1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      allies <- select $ HealableAsset source #damage $ #ally <> assetControlledBy iid
      canHealSelf <- iid <=~> HealableInvestigator source #damage Anyone
      chooseOrRunOneM iid do
        when canHealSelf do
          labeled "Heal 1 damage from yourself" $ healDamage iid source 1
        targets allies $ healDamageOn source 1
      pure a
    _ -> ExtraRations1 <$> liftRunMessage msg attrs
