module Arkham.Homebrew.AgesUnwound.Treacheries.EnemyOfMyEnemy (enemyOfMyEnemy) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Missions.Helpers (placeThisTask)
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype EnemyOfMyEnemy = EnemyOfMyEnemy TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enemyOfMyEnemy :: TreacheryCard EnemyOfMyEnemy
enemyOfMyEnemy = treachery EnemyOfMyEnemy Cards.enemyOfMyEnemy

{- | "__Revelation__ - Spawn the set-aside Savage Yeti enemy at the Himalayas. /
__Task__ - Deal with the Savage Yeti."

The objective has no mechanical trigger of its own: /Gratitude/, the Savage
Yeti's back, is the only thing that completes this Task.
-}
instance RunMessage EnemyOfMyEnemy where
  runMessage msg t@(EnemyOfMyEnemy attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      placeThisTask attrs
      createSetAsideEnemy_ Enemies.savageYeti (locationIs Locations.himalayas)
      pure t
    _ -> EnemyOfMyEnemy <$> liftRunMessage msg attrs
