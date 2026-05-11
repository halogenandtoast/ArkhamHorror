module Arkham.Treachery.Cards.PulledIn (pulledIn) where

import Arkham.Attack (enemyAttack)
import Arkham.EnemyLocation.Types (enemyLocationAsEnemyId)
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Trait (Trait (Monster))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted
import {-# SOURCE #-} Arkham.Game.Utils (maybeEnemyLocation)

newtype PulledIn = PulledIn TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pulledIn :: TreacheryCard PulledIn
pulledIn = treachery PulledIn Cards.pulledIn

instance RunMessage PulledIn where
  runMessage msg t@(PulledIn attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      anyEnemyLocations <- selectAny $ LocationWithTrait Monster
      if not anyEnemyLocations
        then gainSurge attrs
        else do
          sid <- getRandom
          revelationSkillTest sid iid attrs #combat (Fixed 4)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      mTarget <- selectOne $ NearestLocationTo iid (LocationWithTrait Monster)
      for_ mTarget \lid -> do
        moveTowards attrs iid lid
        when (n >= 4) do
          mEl <- maybeEnemyLocation lid
          for_ mEl \el ->
            push $ InitiateEnemyAttack $ enemyAttack (enemyLocationAsEnemyId el) attrs iid
      pure t
    _ -> PulledIn <$> liftRunMessage msg attrs
