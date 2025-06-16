module Arkham.Asset.Assets.ThePaleLanternBeguilingAura (
  thePaleLanternBeguilingAura,
  ThePaleLanternBeguilingAura(..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose

newtype ThePaleLanternBeguilingAura = ThePaleLanternBeguilingAura AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Beguiling Aura side of The Pale Lantern (#71068b).
thePaleLanternBeguilingAura :: AssetCard ThePaleLanternBeguilingAura
thePaleLanternBeguilingAura =
  storyAsset ThePaleLanternBeguilingAura Cards.thePaleLanternBeguilingAura

instance HasAbilities ThePaleLanternBeguilingAura where
  getAbilities (ThePaleLanternBeguilingAura a) =
    [ mkAbility a 1 $ forced $ AssetLeavesPlay #when (be a)
    , restrictedAbility a 2 Here actionAbility
    , controlledAbility a 3 ControlsThis $ actionAbilityWithCost (exhaust a)
    ]

instance RunMessage ThePaleLanternBeguilingAura where
  runMessage msg a@(ThePaleLanternBeguilingAura attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lid <- fieldJust AssetLocation attrs.id
      flipOverBy iid (attrs.ability 1) attrs
      place attrs.id $ AttachedToLocation lid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assets <- select $ AssetAt (locationWithInvestigator iid)
      spellboundAssets <- filterM (\aid -> do
        meta <- field AssetMetaMap aid
        pure $ fromMaybe False $ meta ^? ix "spellbound" . _Bool)
        assets
      chooseOrRunOne iid
        [ targetLabel aid [Flip iid (attrs.ability 2) (AssetTarget aid)]
        | aid <- spellboundAssets
        ]
      pure a
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      sid <- getRandom
      chooseOne iid
        [ Label "Use your {combat}" $ beginSkillTest sid iid (attrs.ability 3) iid #combat (Fixed 2)
        , Label "Use your {agility}" $ beginSkillTest sid iid (attrs.ability 3) iid #agility (Fixed 2)
        ]
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 3 -> True) -> do
      push $ PlaceDamage (attrs.ability 3) (toTarget attrs) 1
      afterDamage <- field AssetDamage attrs.id
      when (afterDamage >= 4) do
        push $ AddToVictory (AssetTarget attrs.id)
        allAssets <- selectList AnyAsset
        spellboundAssets <- filterM (\aid -> do
          meta <- field AssetMetaMap aid
          pure $ fromMaybe False $ meta ^? ix "spellbound" . _Bool) allAssets
        for_ spellboundAssets $ \aid -> flipOverBy iid (attrs.ability 3) aid
      pure a
    _ -> ThePaleLanternBeguilingAura <$> liftRunMessage msg attrs
