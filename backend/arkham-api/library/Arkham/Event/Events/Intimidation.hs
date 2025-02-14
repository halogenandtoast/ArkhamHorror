module Arkham.Event.Events.Intimidation (intimidation) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Enemy.Types (Field(..))
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy)
import Arkham.Deck qualified as Deck

newtype Intimidation = Intimidation EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intimidation :: EventCard Intimidation
intimidation = event Intimidation Cards.intimidation

instance RunMessage Intimidation where
  runMessage msg e@(Intimidation attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectOneToHandle iid attrs $ enemyAtLocationWith iid <> EnemyWithRemainingHealth (atLeast 1)
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget eid) -> do
      sid <- getRandom
      beginSkillTest sid iid attrs eid #combat (EnemyMaybeFieldCalculation eid EnemyRemainingHealth)
      pure e
    PassedThisSkillTest _iid (isSource attrs -> True) -> do
      getSkillTestTargetedEnemy >>= traverse_ \enemy -> do
        elite <- enemy <=~> EliteEnemy
        if elite
          then nonAttackEnemyDamage attrs 2 enemy
          else shuffleIntoDeck Deck.EncounterDeck enemy

      pure e
    _ -> Intimidation <$> liftRunMessage msg attrs
