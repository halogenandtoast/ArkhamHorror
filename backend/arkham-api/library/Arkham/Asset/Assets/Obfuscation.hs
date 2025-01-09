module Arkham.Asset.Assets.Obfuscation (obfuscation) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Window (getAttackDetails)
import Arkham.Matcher

newtype Obfuscation = Obfuscation AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obfuscation :: AssetCard Obfuscation
obfuscation = asset Obfuscation Cards.obfuscation

instance HasAbilities Obfuscation where
  getAbilities (Obfuscation x) =
    [ restricted x 1 ControlsThis
        $ ReactionAbility (EnemyWouldAttack #when You AttackOfOpportunityAttack AnyEnemy)
        $ assetUseCost x Charge 1
    ]

instance RunMessage Obfuscation where
  runMessage msg a@(Obfuscation attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getAttackDetails -> details) _ -> do
      cancelAttack (attrs.ability 1) details
      pure a
    _ -> Obfuscation <$> liftRunMessage msg attrs
