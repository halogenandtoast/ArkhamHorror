module Arkham.Treachery.Cards.TheInnsmouthConspiracy.ThePitOfDespair.FromTheDepths (fromTheDepths, FromTheDepths (..)) where

import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.ThePitOfDespair qualified as Enemies
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Treachery.CardDefs.TheInnsmouthConspiracy.ThePitOfDespair qualified as Cards
import Arkham.Treachery.Import.Lifted
import Arkham.Zone

newtype FromTheDepths = FromTheDepths TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fromTheDepths :: TreacheryCard FromTheDepths
fromTheDepths = treachery FromTheDepths Cards.fromTheDepths

instance RunMessage FromTheDepths where
  runMessage msg t@(FromTheDepths attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      selectOne (OutOfPlayEnemy TheDepths $ enemyIs Enemies.theAmalgam) >>= \case
        Just theAmalgam -> do
          withLocationOf iid \lid -> push $ EnemySpawnFromOutOfPlay TheDepths (Just iid) lid theAmalgam
        Nothing ->
          selectOne (enemyIs Enemies.theAmalgam) >>= traverse_ \theAmalgam -> do
            push $ PlaceEnemyOutOfPlay TheDepths theAmalgam
            gainSurge attrs
      pure t
    _ -> FromTheDepths <$> liftRunMessage msg attrs
