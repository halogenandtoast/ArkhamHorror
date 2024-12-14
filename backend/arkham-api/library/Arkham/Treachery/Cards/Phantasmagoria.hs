module Arkham.Treachery.Cards.Phantasmagoria (phantasmagoria) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyCardsUnderneath, EnemyLocation))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Phantasmagoria = Phantasmagoria TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

phantasmagoria :: TreacheryCard Phantasmagoria
phantasmagoria = treachery Phantasmagoria Cards.phantasmagoria

instance RunMessage Phantasmagoria where
  runMessage msg t@(Phantasmagoria attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      allSeepingNightmares <- selectWithField EnemyCardsUnderneath (enemyIs Enemies.seepingNightmare)
      if notNull allSeepingNightmares
        then do
          closestSeepingNightmares <-
            selectWithField
              EnemyCardsUnderneath
              (NearestEnemyToAnInvestigator $ enemyIs Enemies.seepingNightmare)
          let seepingNightmares = if null closestSeepingNightmares then allSeepingNightmares else closestSeepingNightmares

          chooseOneM iid do
            for_ seepingNightmares \(x, cards) -> do
              targeting x $ case cards of
                [] -> do
                  push $ HunterMove x
                  temporaryModifier x attrs DoNotExhaust do
                    push $ ForTarget (toTarget x) EnemiesAttack
                (n : _) ->
                  field EnemyLocation x >>= traverse_ \lid -> do
                    obtainCard n
                    createEnemy_ n lid
        else gainSurge attrs

      pure t
    _ -> Phantasmagoria <$> liftRunMessage msg attrs
