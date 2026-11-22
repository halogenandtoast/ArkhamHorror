module Arkham.Asset.Assets.SomniumShotcaster4 (somniumShotcaster4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype SomniumShotcaster4 = SomniumShotcaster4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

somniumShotcaster4 :: AssetCard SomniumShotcaster4
somniumShotcaster4 = asset SomniumShotcaster4 Cards.somniumShotcaster4

instance HasAbilities SomniumShotcaster4 where
  getAbilities (SomniumShotcaster4 a) =
    [ controlled a 1 (exists $ EnemyAt Anywhere) $ freeReaction (AssetEntersPlay #when $ be a)
    , controlled_ a 2 $ fightAction (assetUseCost a Ammo 1)
    ]

instance RunMessage SomniumShotcaster4 where
  runMessage msg a@(SomniumShotcaster4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ EnemyAt Anywhere
      chooseTargetM iid enemies $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseFightEnemyWithModifiers sid iid (attrs.ability 2) [SkillModifier #combat 2, DamageDealt 1]
      pure a
    _ -> SomniumShotcaster4 <$> liftRunMessage msg attrs
