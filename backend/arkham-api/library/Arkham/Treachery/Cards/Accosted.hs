module Arkham.Treachery.Cards.Accosted (accosted) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Accosted = Accosted TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

accosted :: TreacheryCard Accosted
accosted = treachery Accosted Cards.accosted

instance RunMessage Accosted where
  runMessage msg t@(Accosted attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      cultists <- select $ NearestEnemyToFallback iid (InPlayEnemy #cultist)
      if null cultists
        then do
          assignDamage iid attrs 1
          gainSurge attrs
        else do
          sid <- getRandom
          chooseTargetM iid cultists \x -> do
            placeDoom attrs x 1
            beginSkillTest sid iid attrs iid #agility (EnemyMaybeFieldCalculation x EnemyEvade)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 2
      pure t
    _ -> Accosted <$> liftRunMessage msg attrs
