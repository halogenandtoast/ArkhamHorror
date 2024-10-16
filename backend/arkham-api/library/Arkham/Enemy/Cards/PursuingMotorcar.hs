module Arkham.Enemy.Cards.PursuingMotorcar (pursuingMotorcar, PursuingMotorcar (..)) where

import Arkham.Ability
import Arkham.Attack.Types
import Arkham.Classes.HasQueue (evalQueueT)
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Modifier

newtype PursuingMotorcar = PursuingMotorcar EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pursuingMotorcar :: EnemyCard PursuingMotorcar
pursuingMotorcar =
  enemyWith
    PursuingMotorcar
    Cards.pursuingMotorcar
    (4, Static 4, 2)
    (2, 0)
    (spawnAtL ?~ SpawnAt RearmostLocation)

instance HasAbilities PursuingMotorcar where
  getAbilities (PursuingMotorcar a) =
    extend
      a
      [ mkAbility a 1
          $ forced
          $ EnemyAttacks #when (You <> not_ (InVehicleMatching AnyAsset)) AnyEnemyAttack (be a)
      , restrictedAbility a 2 (exists $ not_ You <> InVehicleMatching (VehicleWithInvestigator You))
          $ forced
          $ EnemyAttacks
            #when
            (You <> InVehicleMatching AnyAsset)
            (not_ $ AttackViaSource (SourceIs $ a.ability 2))
            (be a)
      ]

instance RunMessage PursuingMotorcar where
  runMessage msg e@(PursuingMotorcar attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      enemyAttackModifier (attrs.ability 1) attrs (DamageDealt 2)
      pure e
    UseCardAbility iid (isSource attrs -> True) 2 (getAttackDetails -> details) _ -> do
      others <-
        select
          $ not_ (InvestigatorWithId iid)
          <> InVehicleMatching (VehicleWithInvestigator $ InvestigatorWithId iid)
      msgs <- evalQueueT $ for_ others $ initiateEnemyAttack attrs (attrs.ability 2)
      push $ ChangeEnemyAttackDetails attrs.id $ details {attackAfter = msgs}
      pure e
    _ -> PursuingMotorcar <$> liftRunMessage msg attrs
