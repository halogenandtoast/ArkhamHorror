module Arkham.Act.Cards.Bloodbath (bloodbath) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Keyword (Keyword (Patrol))
import Arkham.Matcher
import Arkham.Ability
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Monster))

newtype Bloodbath = Bloodbath ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodbath :: ActCard Bloodbath
bloodbath = act (3, A) Bloodbath Cards.bloodbath Nothing

instance HasModifiersFor Bloodbath where
  getModifiersFor (Bloodbath a) = do
    modifySelect
      a
      (EnemyWithTitle "Possessed Extra")
      [ EnemyFight 2
      , HealthModifier 2
      , AddKeyword (Patrol $ LocationWithEnemy $ enemyIs Enemies.theContessaEnraged)
      , AddTrait Monster
      ]

instance HasAbilities Bloodbath where
  getAbilities = actAbilities \a ->
    [ fastAbility a 1 (clueCost 1) (exists (at_ YourLocation <> EnemyCanBeDamagedBySource (a.ability 1)))
    , mkAbility a 2 $ Objective $ forced $ ifEnemyDefeated Enemies.theContessaEnraged
    ]

instance RunMessage Bloodbath where
  runMessage msg a@(Bloodbath attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ at_ (locationWithInvestigator iid) <> EnemyCanBeDamagedBySource (attrs.ability 2)
      chooseTargetM iid enemies $ nonAttackEnemyDamage (Just iid) (attrs.ability 2) 1
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R4
      pure a
    _ -> Bloodbath <$> liftRunMessage msg attrs
