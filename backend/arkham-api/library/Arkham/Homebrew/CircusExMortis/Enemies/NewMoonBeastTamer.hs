module Arkham.Homebrew.CircusExMortis.Enemies.NewMoonBeastTamer (newMoonBeastTamer) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.ForMovement
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.Window (getAttackDetails)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Matcher
import Arkham.Trait (Trait (Creature, Monster))

newtype NewMoonBeastTamer = NewMoonBeastTamer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newMoonBeastTamer :: EnemyCard NewMoonBeastTamer
newMoonBeastTamer = enemy NewMoonBeastTamer Cards.newMoonBeastTamer

instance HasAbilities NewMoonBeastTamer where
  getAbilities (NewMoonBeastTamer a) =
    extend1 a
      $ restricted a 1 (thisExists a $ ReadyEnemy <> UnengagedEnemy)
      $ forced
      $ EnemyAttacks #when Anyone AnyEnemyAttack
      $ at_ (orConnected NotForMovement $ locationWithEnemy a.id)

instance RunMessage NewMoonBeastTamer where
  runMessage msg e@(NewMoonBeastTamer attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getAttackDetails -> details) _ -> do
      beast <- details.enemy <=~> oneOf [EnemyWithTrait Creature, EnemyWithTrait Monster]
      enemyAttackModifiers (attrs.ability 1) details.enemy
        $ DamageDealt 1
        : [HorrorDealt 1 | beast]
      pure e
    _ -> NewMoonBeastTamer <$> liftRunMessage msg attrs
