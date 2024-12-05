module Arkham.Asset.Assets.HuntersArmor (huntersArmor, HuntersArmor (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Customization
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window (getDamageOrHorrorSource)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype HuntersArmor = HuntersArmor AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor HuntersArmor where
  getModifiersFor (HuntersArmor a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> do
      self <-
        modifySelf a
          $ (guard (a `hasCustomization` Enchanted) *> [DoNotTakeUpSlot #body, AdditionalSlot #arcane])
          <> (guard (a `hasCustomization` Durable) $> HealthModifier 2)
          <> (guard (a `hasCustomization` Hallowed) $> SanityModifier 2)
          <> ( guard (a `hasCustomization` Lightweight)
                *> [ReduceCostOf (CardWithId a.cardId) 1, ActionDoesNotCauseAttacksOfOpportunity #play]
             )
      other <-
        modifySelectWhen
          a
          (a `hasCustomization` ProtectiveRunes)
          (not_ (InvestigatorWithId iid) <> at_ (locationWithAsset a))
          [CanAssignDamageToAsset a.id, CanAssignHorrorToAsset a.id]
      pure $ self <> other

huntersArmor :: AssetCard HuntersArmor
huntersArmor = assetWith HuntersArmor Cards.huntersArmor $ (healthL ?~ 2) . (sanityL ?~ 2)

instance HasAbilities HuntersArmor where
  getAbilities (HuntersArmor a) =
    [ restrictedAbility a 1 (ControlsThis <> can.draw.cards You <> thisIs a AssetReady)
      $ CustomizationReaction
        "Hexdrinker"
        (AssetDealtDamageOrHorror #after (SourceIsTreacheryEffect AnyTreachery) (be a))
        Free
    | a `hasCustomization` Hexdrinker
    ]
      <> [ restrictedAbility a 2 ControlsThis
          $ CustomizationReaction
            "Armor of Thorns"
            ( AssetDealtDamageOrHorror
                #after
                (SourceIsEnemyAttack $ EnemyCanBeDamagedBySource (a.ability 1))
                (be a)
            )
            (exhaust a)
         | a `hasCustomization` ArmorOfThorns
         ]

instance RunMessage HuntersArmor where
  runMessage msg a@(HuntersArmor attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid do
        labeled "Exhaust Hunter's Armor to draw 1 card" do
          push $ Exhaust (toTarget attrs)
          drawCardsIfCan iid (attrs.ability 1) 1
        labeled "Do no exhaust" nothing
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 (getDamageOrHorrorSource -> EnemyAttackSource enemy) _ -> do
      nonAttackEnemyDamage (attrs.ability 2) 1 enemy
      pure a
    _ -> HuntersArmor <$> liftRunMessage msg attrs
