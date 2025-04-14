module Arkham.Enemy.Cards.SethBishopThrallOfYogSothoth (sethBishopThrallOfYogSothoth) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Modifier

newtype SethBishopThrallOfYogSothoth = SethBishopThrallOfYogSothoth EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sethBishopThrallOfYogSothoth :: EnemyCard SethBishopThrallOfYogSothoth
sethBishopThrallOfYogSothoth = enemy SethBishopThrallOfYogSothoth Cards.sethBishopThrallOfYogSothoth (5, PerPlayer 4, 5) (1, 1)

instance HasAbilities SethBishopThrallOfYogSothoth where
  getAbilities (SethBishopThrallOfYogSothoth a) =
    extend
      a
      [ restricted a 1 (DuringPhase #enemy) $ forced $ EnemyAttacks #when You AnyEnemyAttack (be a)
      , mkAbility a 2
          $ forced
          $ oneOf
            [ EnemyEnters #after (LocationWithEnemy $ enemyIs Cards.yogSothoth) (be a)
            , EnemyEnters #after (locationWithEnemy a) (enemyIs Cards.yogSothoth)
            ]
      ]

instance RunMessage SethBishopThrallOfYogSothoth where
  runMessage msg e@(SethBishopThrallOfYogSothoth attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid do
        labeled "Move to Another Dimension after this attack" do
          afterEnemyAttack attrs do
            moveTo (attrs.ability 1) iid =<< selectJust (locationIs Locations.anotherDimension)
        labeled "Seth Bishop deals +2 damage for this attack." do
          enemyAttackModifier (attrs.ability 1) attrs (DamageDealt 2)
      pure e
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      removeFromGame attrs
      selectOne (enemyIs Cards.yogSothoth)
        >>= traverse_ \yog -> healDamage yog (attrs.ability 2) 6
      pure e
    _ -> SethBishopThrallOfYogSothoth <$> liftRunMessage msg attrs
