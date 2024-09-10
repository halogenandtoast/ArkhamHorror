module Arkham.Event.Cards.EnchantWeapon3 (enchantWeapon3, EnchantWeapon3 (..)) where

import Arkham.Ability
import Arkham.Asset.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.Trait (Trait (Relic))

newtype EnchantWeapon3 = EnchantWeapon3 EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enchantWeapon3 :: EventCard EnchantWeapon3
enchantWeapon3 = event EnchantWeapon3 Cards.enchantWeapon3

instance HasModifiersFor EnchantWeapon3 where
  getModifiersFor target (EnchantWeapon3 a) | target `elem` a.attachedTo = do
    modified a [AddTrait Relic, AdditionalSlot #arcane]
  getModifiersFor _ _ = pure []

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
        selectWithField AssetController
          $ AssetControlledBy (affectsOthers $ colocatedWith iid)
          <> #weapon
          <> not_ (AssetWithAttachedEvent $ eventIs Cards.enchantWeapon3)
      chooseOne
        iid
        [ targetLabel asset [PlaceEvent attrs.id $ AttachedToAsset asset Nothing, RefillSlots owner]
        | (asset, Just owner) <- assets
        ]

      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      owner <- field EventOwner (toId attrs)
      nextSkillTestModifiers (attrs.ability 1) iid [AddSkillValueOf #willpower owner, DamageDealt 1]
      pure e
    _ -> EnchantWeapon3 <$> liftRunMessage msg attrs
