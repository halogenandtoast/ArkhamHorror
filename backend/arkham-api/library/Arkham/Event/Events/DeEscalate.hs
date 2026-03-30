module Arkham.Event.Events.DeEscalate (deEscalate) where

import Arkham.Enemy.Types (Field (EnemySanityDamage))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Projection

newtype DeEscalate = DeEscalate EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deEscalate :: EventCard DeEscalate
deEscalate = event DeEscalate Cards.deEscalate

instance RunMessage DeEscalate where
  runMessage msg e@(DeEscalate attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      withLocationOf iid \location -> do
        enemies <- select $ enemyAt location <> canParleyEnemy iid <> EnemyWithHorrorValue
        chooseTargetM iid enemies $ field EnemySanityDamage >=> healHorror iid attrs
      pure e
    _ -> DeEscalate <$> liftRunMessage msg attrs
