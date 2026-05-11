module Arkham.Asset.Assets.OldHuntingRifle3 (oldHuntingRifle3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.ChaosToken
import Arkham.Helpers.SkillTest (getSkillTestSource)
import Arkham.I18n
import Arkham.Message qualified as Msg
import Arkham.Modifier

data RifleStatus = NotJammed | Jammed
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Metadata = Metadata {rifleStatus :: RifleStatus}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype OldHuntingRifle3 = OldHuntingRifle3 (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldHuntingRifle3 :: AssetCard OldHuntingRifle3
oldHuntingRifle3 = asset (OldHuntingRifle3 . (`with` Metadata NotJammed)) Cards.oldHuntingRifle3

instance HasAbilities OldHuntingRifle3 where
  getAbilities (OldHuntingRifle3 (a `With` Metadata rifleStatus)) =
    fightAbility a 1 (assetUseCost a Ammo 1) (ControlsThis <> jammedRestriction)
      : [ cardI18n $ scope "oldHuntingRifle3" $ withI18nTooltip "clearJam" $ controlled_ a 2 actionAbility
        | rifleStatus == Jammed
        ]
   where
    jammedRestriction = case rifleStatus of
      Jammed -> Never
      NotJammed -> NoRestriction

instance RunMessage OldHuntingRifle3 where
  runMessage msg a@(OldHuntingRifle3 (attrs `With` meta)) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 1) iid [SkillModifier #combat 3, DamageDealt 2]
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    Msg.RevealChaosToken (SkillTestSource _) _ t | t.face `elem` [Skull, AutoFail] -> do
      getSkillTestSource >>= \case
        Just (isAbilitySource attrs 1 -> True) -> do
          failSkillTest
          pure . OldHuntingRifle3 $ attrs `with` Metadata Jammed
        _ -> pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      pure . OldHuntingRifle3 $ attrs `with` Metadata NotJammed
    _ -> OldHuntingRifle3 . (`with` meta) <$> liftRunMessage msg attrs
