module Arkham.Event.Events.EnchantWeapon3 (enchantWeapon3) where

import Arkham.Ability
import Arkham.Asset.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Message.Lifted.Upgrade
import Arkham.Projection
import Arkham.Trait (Trait (Relic))

newtype EnchantWeapon3 = EnchantWeapon3 EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enchantWeapon3 :: EventCard EnchantWeapon3
enchantWeapon3 = event EnchantWeapon3 Cards.enchantWeapon3

instance HasModifiersFor EnchantWeapon3 where
  getModifiersFor (EnchantWeapon3 a) = case a.attachedTo of
    Just target -> modified_ a target [AddTrait Relic, AdditionalSlot #arcane]
    _ -> pure mempty

instance HasAbilities EnchantWeapon3 where
  getAbilities (EnchantWeapon3 x) =
    case x.attachedTo of
      Just (AssetTarget aid) ->
        [ mkAbility x 1
            $ ReactionAbility
              (ActivateAbility #when You (AbilityIsAction #fight <> AssetAbility (AssetWithId aid)))
              (exhaust x)
        ]
      _ -> []

instance RunMessage EnchantWeapon3 where
  runMessage msg e@(EnchantWeapon3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      assets <-
        getUpgradeTargets iid
          $ AssetControlledBy (affectsOthers $ colocatedWith iid)
          <> #weapon
          <> not_ (AssetWithAttachedEvent $ eventIs Cards.enchantWeapon3)
      chooseOneM iid do
        for_ assets \asset ->
          field AssetController asset >>= traverse_ \controller ->
            targeting asset do
              place attrs $ AttachedToAsset asset Nothing
              push $ RefillSlots controller []
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      owner <- field EventOwner (toId attrs)
      nextSkillTestModifiers iid (attrs.ability 1) iid [AddSkillValueOf #willpower owner, DamageDealt 1]
      pure e
    _ -> EnchantWeapon3 <$> liftRunMessage msg attrs
