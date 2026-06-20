module Arkham.Enemy.Cards.Subject8L08EpicMultiplayer (subject8L08EpicMultiplayer) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype Subject8L08EpicMultiplayer = Subject8L08EpicMultiplayer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

-- Epic Multiplayer variant. Subject 8L-08 has a global health pool tracked by
-- the event organizer, so it has no standard fight, evade, health, damage, or
-- horror values in the engine.
subject8L08EpicMultiplayer :: EnemyCard Subject8L08EpicMultiplayer
subject8L08EpicMultiplayer =
  enemyWith Subject8L08EpicMultiplayer Cards.subject8L08EpicMultiplayer
    $ \a -> a {enemyFight = Nothing, enemyEvade = Nothing, enemyHealth = Nothing}

instance RunMessage Subject8L08EpicMultiplayer where
  runMessage msg (Subject8L08EpicMultiplayer attrs) =
    Subject8L08EpicMultiplayer <$> runMessage msg attrs
