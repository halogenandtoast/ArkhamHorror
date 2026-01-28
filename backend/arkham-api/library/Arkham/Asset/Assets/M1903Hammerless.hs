module Arkham.Asset.Assets.M1903Hammerless (m1903Hammerless) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Modifier

newtype M1903Hammerless = M1903Hammerless AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

m1903Hammerless :: AssetCard M1903Hammerless
m1903Hammerless = asset M1903Hammerless Cards.m1903Hammerless

instance HasAbilities M1903Hammerless where
  getAbilities (M1903Hammerless a) = [skillTestAbility $ controlled_ a 1 $ fightActionWith #agility $ assetUseCost a Ammo 1]

instance RunMessage M1903Hammerless where
  runMessage msg a@(M1903Hammerless attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseFightEnemyWith #agility sid iid (attrs.ability 1)
      pure a
    ChoseEnemy sid iid (isAbilitySource attrs 1 -> True) enemy -> do
      skillTestModifier sid (attrs.ability 1) iid
        $ CriteriaModifier (exists $ EnemyWithId enemy <> #exhausted) (DamageDealt 1)
      pure a
    _ -> M1903Hammerless <$> liftRunMessage msg attrs
