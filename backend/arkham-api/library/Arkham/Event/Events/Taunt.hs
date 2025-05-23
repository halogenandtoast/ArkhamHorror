module Arkham.Event.Events.Taunt (taunt) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator

newtype Taunt = Taunt EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

taunt :: EventCard Taunt
taunt = event Taunt Cards.taunt

instance RunMessage Taunt where
  runMessage msg e@(Taunt attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      enemies <- select $ enemiesColocatedWith iid
      chooseSomeM iid "Done engaging enemies" $ targets enemies $ engageEnemy iid
      pure e
    _ -> Taunt <$> liftRunMessage msg attrs
