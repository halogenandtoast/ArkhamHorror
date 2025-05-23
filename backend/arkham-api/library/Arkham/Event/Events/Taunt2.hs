module Arkham.Event.Events.Taunt2 (taunt2) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator

newtype Taunt2 = Taunt2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

taunt2 :: EventCard Taunt2
taunt2 = event Taunt2 Cards.taunt2

instance RunMessage Taunt2 where
  runMessage msg e@(Taunt2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      enemies <- select $ enemiesColocatedWith iid
      chooseSomeM iid "Done engaging enemies" do
        targets enemies \enemy -> do
          engageEnemy iid enemy
          drawCards iid attrs 1
      pure e
    _ -> Taunt2 <$> liftRunMessage msg attrs
