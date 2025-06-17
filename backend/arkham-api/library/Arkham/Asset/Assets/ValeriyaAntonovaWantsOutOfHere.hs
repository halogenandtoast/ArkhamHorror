module Arkham.Asset.Assets.ValeriyaAntonovaWantsOutOfHere (valeriyaAntonovaWantsOutOfHere) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetExhausted)
import Arkham.Effect.Window
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Guest))

newtype ValeriyaAntonovaWantsOutOfHere = ValeriyaAntonovaWantsOutOfHere AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valeriyaAntonovaWantsOutOfHere :: AssetCard ValeriyaAntonovaWantsOutOfHere
valeriyaAntonovaWantsOutOfHere =
  allyWith ValeriyaAntonovaWantsOutOfHere Cards.valeriyaAntonovaWantsOutOfHere (2, 3) noSlots

instance HasModifiersFor ValeriyaAntonovaWantsOutOfHere where
  getModifiersFor (ValeriyaAntonovaWantsOutOfHere a) = controllerGets a [SkillModifier #combat 1]

instance HasAbilities ValeriyaAntonovaWantsOutOfHere where
  getAbilities (ValeriyaAntonovaWantsOutOfHere a) =
    [ restricted a 1 ControlsThis (actionAbilityWithCost $ exhaust a)
    , mkAbility a 2 $ forced $ AssetLeavesPlay #when (be a)
    ]

instance RunMessage ValeriyaAntonovaWantsOutOfHere where
  runMessage msg a@(ValeriyaAntonovaWantsOutOfHere attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      assets <-
        select
          $ AssetWithTrait Guest
          <> AssetExhausted
          <> not_ (be attrs)
          <> at_ (locationWithInvestigator iid)
      chooseTargetM iid assets readyThis
      guestCount <- selectCount $ AssetWithTrait Guest <> at_ (locationWithInvestigator iid)
      when (guestCount > 0) do
        ems <- effectModifiers (attrs.ability 1) [AnySkillValue guestCount]
        push
          $ CreateWindowModifierEffect
            (FirstEffectWindow [EffectNextSkillTestWindow iid, EffectRoundWindow])
            ems
            (attrs.ability 1)
            (toTarget iid)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      removeFromGame attrs
      pure a
    _ -> ValeriyaAntonovaWantsOutOfHere <$> liftRunMessage msg attrs
