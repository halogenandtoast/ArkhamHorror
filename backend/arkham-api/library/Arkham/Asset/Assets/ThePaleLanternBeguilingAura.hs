module Arkham.Asset.Assets.ThePaleLanternBeguilingAura (thePaleLanternBeguilingAura) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Modifier
import Arkham.Projection

newtype ThePaleLanternBeguilingAura = ThePaleLanternBeguilingAura AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Beguiling Aura side of The Pale Lantern (#71068b).
thePaleLanternBeguilingAura :: AssetCard ThePaleLanternBeguilingAura
thePaleLanternBeguilingAura = asset ThePaleLanternBeguilingAura Cards.thePaleLanternBeguilingAura

instance HasAbilities ThePaleLanternBeguilingAura where
  getAbilities (ThePaleLanternBeguilingAura a) =
    [ mkAbility a 1 $ forced $ AssetLeavesPlay #when (be a)
    , restricted a 2 OnSameLocation actionAbility
    , restricted a 3 ControlsThis $ actionAbilityWithCost (exhaust a)
    ]

instance RunMessage ThePaleLanternBeguilingAura where
  runMessage msg a@(ThePaleLanternBeguilingAura attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lid <- fieldJust AssetLocation attrs.id
      flipOverBy iid (attrs.ability 1) attrs
      place attrs.id $ AttachedToLocation lid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      spellbound <-
        select $ AssetAt (locationWithInvestigator iid) <> AssetWithModifier (ScenarioModifier "spellbound")
      chooseOrRunOneM iid $ targets spellbound $ flipOverBy iid (attrs.ability 2)
      pure a
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#combat, #agility] \kind ->
          skillLabeled kind $ beginSkillTest sid iid (attrs.ability 3) iid kind (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 3 -> True) -> do
      placeTokens (attrs.ability 3) attrs #damage 1
      afterDamage <- field AssetDamage attrs.id
      when (afterDamage >= 4) do
        addToVictory attrs
        spellbound <-
          select $ AssetAt (locationWithInvestigator iid) <> AssetWithModifier (ScenarioModifier "spellbound")
        for_ spellbound $ flipOverBy iid (attrs.ability 3)
      pure a
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.thePaleLanternHypnoticGlow
      pure a
    _ -> ThePaleLanternBeguilingAura <$> liftRunMessage msg attrs
