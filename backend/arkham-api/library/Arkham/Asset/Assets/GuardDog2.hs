module Arkham.Asset.Assets.GuardDog2 (GuardDog2 (..), guardDog2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Attack
import Arkham.Helpers.Window
import Arkham.Matcher hiding (NonAttackDamageEffect)

newtype GuardDog2 = GuardDog2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guardDog2 :: AssetCard GuardDog2
guardDog2 = ally GuardDog2 Cards.guardDog2 (4, 2)

instance HasAbilities GuardDog2 where
  getAbilities (GuardDog2 x) =
    [ controlledAbility x 1 (exists (at_ YourLocation <> CanEngageEnemy (x.ability 1)))
        $ FastAbility (exhaust x)
    , controlledAbility x 2 CanDealDamage
        $ freeReaction
        $ AssetDealtDamageOrHorror #when (SourceIsEnemyAttack AnyEnemy) (be x)
    ]

instance RunMessage GuardDog2 where
  runMessage msg a@(GuardDog2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectOrRunOneToHandle iid (attrs.ability 1)
        $ at_ (locationWithInvestigator iid)
        <> CanEngageEnemy (toSource attrs)
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (EnemyTarget enemy) -> do
      push $ EngageEnemy iid enemy Nothing False
      push $ InitiateEnemyAttack $ enemyAttack enemy attrs iid
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 (getDamageOrHorrorSource -> (.enemy) -> Just eid) _ -> do
      nonAttackEnemyDamage (attrs.ability 2) 1 eid
      pure a
    _ -> GuardDog2 <$> liftRunMessage msg attrs
