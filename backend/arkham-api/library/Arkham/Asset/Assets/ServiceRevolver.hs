module Arkham.Asset.Assets.ServiceRevolver (serviceRevolver) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyAttacks)
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.Window (getAttackDetails)
import Arkham.Matcher

newtype ServiceRevolver = ServiceRevolver AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

serviceRevolver :: AssetCard ServiceRevolver
serviceRevolver = asset ServiceRevolver Cards.serviceRevolver

instance HasAbilities ServiceRevolver where
  getAbilities (ServiceRevolver a) =
    [ skillTestAbility
        $ controlled_ a 1
        $ triggeredAction
          #fight
          (EnemyAttacks #after You AnyEnemyAttack AnyEnemy)
          (assetUseCost a Ammo 1)
    ]

instance RunMessage ServiceRevolver where
  runMessage msg a@(ServiceRevolver attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getAttackDetails -> attack) _ -> do
      sid <- getRandom
      skillTestModifiers sid attrs iid [SkillModifier #combat 1, DamageDealt 1]
      chooseFightEnemyMatch sid iid (attrs.ability 1) (fightOverride $ EnemyWithId attack.enemy)
      pure a
    _ -> ServiceRevolver <$> liftRunMessage msg attrs
