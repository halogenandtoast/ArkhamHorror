module Arkham.Event.Events.HandOfFate (handOfFate, HandOfFate (..)) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Window
import Arkham.Projection

newtype HandOfFate = HandOfFate EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

handOfFate :: EventCard HandOfFate
handOfFate = event HandOfFate Cards.handOfFate

instance RunMessage HandOfFate where
  runMessage msg e@(HandOfFate attrs) = runQueueT $ case msg of
    PlayThisEvent _iid (is attrs -> True) -> do
      let currentAttack = getAttackDetails attrs.windows
      damage <- field EnemyHealthDamage currentAttack.enemy
      horror <- field EnemySanityDamage currentAttack.enemy
      n <- max (damage + horror) <$> getRemainingBlessTokens
      cancelAttack attrs currentAttack
      replicateM_ n $ addChaosToken #bless
      pure e
    _ -> HandOfFate <$> liftRunMessage msg attrs
