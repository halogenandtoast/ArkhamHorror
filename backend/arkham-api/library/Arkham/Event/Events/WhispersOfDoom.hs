module Arkham.Event.Events.WhispersOfDoom (whispersOfDoom) where

import Arkham.Calculation
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy)
import Arkham.Matcher

newtype WhispersOfDoom = WhispersOfDoom EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whispersOfDoom :: EventCard WhispersOfDoom
whispersOfDoom = event WhispersOfDoom Cards.whispersOfDoom

instance RunMessage WhispersOfDoom where
  runMessage msg e@(WhispersOfDoom attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      enemies <- select $ enemyAtLocationWith iid <> NonWeaknessEnemy
      sid <- getRandom
      chooseTargetM iid enemies \enemy -> do
        beginSkillTest sid iid attrs enemy #willpower
          $ SumCalculation [Fixed 3, EnemyFieldCalculation enemy EnemySanityDamage]
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      getSkillTestTargetedEnemy >>= traverse_ \enemy -> do
        elite <- enemy <=~> EliteEnemy
        if elite
          then nonAttackEnemyDamage attrs 3 enemy
          else defeatEnemy enemy iid attrs
      pure e
    FailedThisSkillTest _iid (isSource attrs -> True) -> do
      getSkillTestTargetedEnemy >>= traverse_ \enemy -> placeDoom attrs enemy 1
      pure e
    _ -> WhispersOfDoom <$> liftRunMessage msg attrs
