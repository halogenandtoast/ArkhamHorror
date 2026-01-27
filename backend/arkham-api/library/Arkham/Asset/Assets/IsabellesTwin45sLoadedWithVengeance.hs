module Arkham.Asset.Assets.IsabellesTwin45sLoadedWithVengeance (isabellesTwin45sLoadedWithVengeance) where

import Arkham.Ability
import Arkham.Fight (withSkillType)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Modifier

newtype IsabellesTwin45sLoadedWithVengeance = IsabellesTwin45sLoadedWithVengeance AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities IsabellesTwin45sLoadedWithVengeance where
  getAbilities (IsabellesTwin45sLoadedWithVengeance a) =
    [ skillTestAbility $ controlled_ a 1 $ fightAction (assetUseCost a Ammo 1)
    , skillTestAbility
        $ controlled_ a 2
        $ triggeredAction
          #fight
          (ActivateAbility #after You (AbilityIs (toSource a) 1))
          (exhaust a <> assetUseCost a Ammo 1)
    ]

isabellesTwin45sLoadedWithVengeance :: AssetCard IsabellesTwin45sLoadedWithVengeance
isabellesTwin45sLoadedWithVengeance = asset IsabellesTwin45sLoadedWithVengeance Cards.isabellesTwin45sLoadedWithVengeance

instance RunMessage IsabellesTwin45sLoadedWithVengeance where
  runMessage msg a@(IsabellesTwin45sLoadedWithVengeance attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 1) iid [SkillModifier #combat 1, DamageDealt 1]
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 1) iid [SkillModifier #agility 1, DamageDealt 1]
      chooseFightEnemyEdit sid iid (attrs.ability 1) (withSkillType #agility)
      pure a
    _ -> IsabellesTwin45sLoadedWithVengeance <$> liftRunMessage msg attrs
