module Arkham.Treachery.Cards.MeddlesomeFamiliar (meddlesomeFamiliar) where

import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Enemy
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MeddlesomeFamiliar = MeddlesomeFamiliar TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

meddlesomeFamiliar :: TreacheryCard MeddlesomeFamiliar
meddlesomeFamiliar = treachery MeddlesomeFamiliar Cards.meddlesomeFamiliar

instance RunMessage MeddlesomeFamiliar where
  runMessage msg t@(MeddlesomeFamiliar attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      brownJenkinInPlay <- getEnemyIsInPlay Enemies.brownJenkin
      findEncounterCard iid attrs $ if brownJenkinInPlay then Enemies.swarmOfRats else Enemies.brownJenkin
      pure t
    FoundEncounterCard iid (isTarget attrs -> True) (toCard -> card)
      | card `cardMatch` Enemies.brownJenkin -> do
          createEnemy_ card (locationWithInvestigator iid)
          assignDamage iid attrs 1
          pure t
      | card `cardMatch` Enemies.swarmOfRats -> do
          createEnemyWithAfter_ card iid \_ -> assignDamage iid attrs 1
          pure t
    _ -> MeddlesomeFamiliar <$> liftRunMessage msg attrs
