module Arkham.Treachery.Cards.PulledIn (pulledIn) where

import Arkham.EnemyLocation.Types (enemyLocationAsEnemyId)
import {-# SOURCE #-} Arkham.Game.Utils (maybeEnemyLocation)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Movement
import Arkham.Trait (Trait (Monster))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

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
      locations <- select $ NearestLocationTo iid (LocationWithTrait Monster)
      chooseTargetM iid locations \lid -> do
        moveToEdit attrs iid lid \m -> m {moveMeans = OneAtATime}
        when (n >= 4) do
          whenJustM (maybeEnemyLocation lid) \el -> do
            initiateEnemyAttack (enemyLocationAsEnemyId el) attrs iid
      pure t
    _ -> PulledIn <$> liftRunMessage msg attrs
