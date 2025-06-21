module Arkham.Asset.Assets.ThePaleLanternBeguilingAura (thePaleLanternBeguilingAura) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Projection
import Arkham.Scenarios.TheMidwinterGala.Helpers
import Arkham.Helpers.Location (withLocationOf)

newtype ThePaleLanternBeguilingAura = ThePaleLanternBeguilingAura AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePaleLanternBeguilingAura :: AssetCard ThePaleLanternBeguilingAura
thePaleLanternBeguilingAura = asset ThePaleLanternBeguilingAura Cards.thePaleLanternBeguilingAura

instance HasAbilities ThePaleLanternBeguilingAura where
  getAbilities (ThePaleLanternBeguilingAura a) =
    [ scenarioI18n
        $ withI18nTooltip "thePaleLanternBeguilingAura.flip"
        $ controlled a 1 (exists $ SpellboundAsset $ AssetAt YourLocation) actionAbility
    , scenarioI18n
        $ withI18nTooltip "thePaleLanternBeguilingAura.break"
        $ restricted a 2 ControlsThis
        $ actionAbilityWithCost (exhaust a)
    , mkAbility a 3 $ forced $ AssetLeavesPlay #when (be a)
    , mkAbility a 4 $ forced $ EnemyLeavesPlay #when $ EnemyWithAttachedAsset (be a)
    ]

instance RunMessage ThePaleLanternBeguilingAura where
  runMessage msg a@(ThePaleLanternBeguilingAura attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      spellbound <- select $ SpellboundAsset $ at_ (locationWithInvestigator iid)
      chooseOrRunOneM iid $ targets spellbound $ flipOverBy iid (attrs.ability 2)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 3) iid [#combat, #agility] (Fixed 2)
      pure a
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      lid <- fieldJust AssetLocation attrs.id
      flipOverBy iid (attrs.ability 1) attrs
      place attrs.id $ AttachedToLocation lid
      pure a
    UseThisAbility _ (isSource attrs -> True) 4 -> do
      case attrs.placement of
        AttachedToEnemy enemyId -> withLocationOf enemyId \lid -> place attrs.id (AttachedToLocation lid)
        _ -> pure ()
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 3 -> True) -> do
      placeTokens (attrs.ability 3) attrs #damage 1
      when (attrs.damage >= 3) do
        addToVictory attrs
        selectEach (SpellboundAsset $ at_ (locationWithInvestigator iid))
          $ flipOverBy iid (attrs.ability 3)
      pure a
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.thePaleLanternHypnoticGlow
      pure a
    _ -> ThePaleLanternBeguilingAura <$> liftRunMessage msg attrs
