module Arkham.Event.Cards.HandOfFate (
  handOfFate,
  HandOfFate (..),
)
where

import Arkham.Attack
import Arkham.Classes
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.ChaosBag
import Arkham.Prelude
import Arkham.Projection

newtype HandOfFate = HandOfFate EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

handOfFate :: EventCard HandOfFate
handOfFate = event HandOfFate Cards.handOfFate

dropUntilAttack :: [Message] -> [Message]
dropUntilAttack = dropWhile (notElem AttackMessage . messageType)

instance RunMessage HandOfFate where
  runMessage msg e@(HandOfFate attrs) = case msg of
    PlayThisEvent _iid eid | eid == toId attrs -> do
      enemyId <- fromQueue $ \queue -> case dropUntilAttack queue of
        PerformEnemyAttack details : _ -> attackEnemy details
        _ -> error "unhandled"
      damage <- field EnemyHealthDamage enemyId
      horror <- field EnemySanityDamage enemyId
      n <- max (damage + horror) <$> getRemainingBlessTokens
      pushAll
        $ CancelNext (toSource attrs) AttackMessage
        : replicate n (AddChaosToken #bless)
      pure e
    _ -> HandOfFate <$> runMessage msg attrs
