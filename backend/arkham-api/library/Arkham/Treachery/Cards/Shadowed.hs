module Arkham.Treachery.Cards.Shadowed (shadowed) where

import Arkham.Enemy.Types
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Shadowed = Shadowed TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shadowed :: TreacheryCard Shadowed
shadowed = treachery Shadowed Cards.shadowed

instance RunMessage Shadowed where
  runMessage msg t@(Shadowed attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      cultists <- select $ NearestEnemyToFallback iid $ EnemyWithTrait Cultist <> EnemyWithFight
      if null cultists
        then do
          assignHorror iid attrs 1
          gainSurge attrs
        else do
          sid <- getRandom
          chooseOrRunOneM iid $ targets cultists \cultist -> do
            placeDoom attrs cultist 1
            revelationSkillTest sid iid attrs #willpower (EnemyMaybeFieldCalculation cultist EnemyFight)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 2
      pure t
    _ -> Shadowed <$> liftRunMessage msg attrs
