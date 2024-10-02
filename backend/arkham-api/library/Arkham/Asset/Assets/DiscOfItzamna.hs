module Arkham.Asset.Assets.DiscOfItzamna where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.DamageEffect
import Arkham.Matcher hiding (EnemyEvaded, NonAttackDamageEffect)
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype DiscOfItzamna = DiscOfItzamna AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

discOfItzamna :: AssetCard DiscOfItzamna
discOfItzamna = asset DiscOfItzamna Cards.discOfItzamna

instance HasAbilities DiscOfItzamna where
  getAbilities (DiscOfItzamna a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (EnemySpawns #when YourLocation NonEliteEnemy) (discardCost a)
    ]

instance RunMessage DiscOfItzamna where
  runMessage msg a@(DiscOfItzamna attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (map windowType -> [Window.EnemySpawns eid _]) _ -> do
      canDamage <- eid <=~> EnemyCanBeDamagedBySource (attrs.ability 1)
      canEvade <- eid <=~> EnemyCanBeEvadedBy (attrs.ability 1)
      when (canDamage || canEvade) do
        chooseOrRunOne
          iid
          [ Label "Evade enemy" [EnemyEvaded iid eid]
          , Label "Deal 2 damage" [EnemyDamage eid $ nonAttack attrs 2]
          ]
      pure a
    _ -> DiscOfItzamna <$> liftRunMessage msg attrs
