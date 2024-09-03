module Arkham.Asset.Cards.Obfuscation (obfuscation, Obfuscation (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype Obfuscation = Obfuscation AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obfuscation :: AssetCard Obfuscation
obfuscation = asset Obfuscation Cards.obfuscation

instance HasAbilities Obfuscation where
  getAbilities (Obfuscation x) =
    [ restrictedAbility x 1 ControlsThis
        $ ReactionAbility (EnemyWouldAttack #when You AttackOfOpportunityAttack AnyEnemy)
        $ assetUseCost x Charge 1
    ]

instance RunMessage Obfuscation where
  runMessage msg a@(Obfuscation attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ CancelNext (toSource attrs) AttackMessage
      pure a
    _ -> Obfuscation <$> runMessage msg attrs
