module Arkham.Event.Events.Taunt (taunt) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator
import Arkham.I18n

newtype Taunt = Taunt EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

taunt :: EventCard Taunt
taunt = event Taunt Cards.taunt

instance RunMessage Taunt where
  runMessage msg e@(Taunt attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      enemies <- select $ enemiesColocatedWith iid
      withI18n $ chooseSomeM iid "doneEngagingEnemies" $ targets enemies $ engageEnemy iid
      pure e
    _ -> Taunt <$> liftRunMessage msg attrs
