module Arkham.Asset.Cards.HuntersArmor (huntersArmor, HuntersArmor (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Game.Helpers (onSameLocation)
import Arkham.Helpers.Customization
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window (getDamageOrHorrorSource)
import Arkham.Matcher

newtype HuntersArmor = HuntersArmor AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor HuntersArmor where
  getModifiersFor target (HuntersArmor a) | isTarget a target = do
    modified a
      $ (guard (a `hasCustomization` Enchanted) *> [DoNotTakeUpSlot #body, AdditionalSlot #arcane])
      <> (guard (a `hasCustomization` Durable) $> HealthModifier 2)
      <> (guard (a `hasCustomization` Hallowed) $> SanityModifier 2)
      <> ( guard (a `hasCustomization` Lightweight)
            *> [ReduceCostOf (CardWithId a.cardId) 1, ActionDoesNotCauseAttacksOfOpportunity #play]
         )
  getModifiersFor (InvestigatorTarget iid) (HuntersArmor a) | not (controlledBy a iid) = do
    sameLocation <- onSameLocation iid a.placement
    modified a
      $ guard (a `hasCustomization` ProtectiveRunes && sameLocation)
      *> [CanAssignDamageToAsset (toId a), CanAssignHorrorToAsset (toId a)]
  getModifiersFor _ _ = pure []

huntersArmor :: AssetCard HuntersArmor
huntersArmor = assetWith HuntersArmor Cards.huntersArmor $ (healthL ?~ 2) . (sanityL ?~ 2)

instance HasAbilities HuntersArmor where
  getAbilities (HuntersArmor a) =
    [ restrictedAbility a 1 (ControlsThis <> can.draw.cards You)
      $ CustomizationReaction
        "Hexdrinker"
        (AssetDealtDamageOrHorror #after (SourceIsTreacheryEffect AnyTreachery) (be a))
        (exhaust a)
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
      drawCardsIfCan iid (attrs.ability 1) 1
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 (getDamageOrHorrorSource -> EnemyAttackSource enemy) _ -> do
      nonAttackEnemyDamage (attrs.ability 2) 1 enemy
      pure a
    _ -> HuntersArmor <$> liftRunMessage msg attrs
