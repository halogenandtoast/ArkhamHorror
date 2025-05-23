module Arkham.Event.Events.Dodge where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Window (getAttackDetails)

newtype Dodge = Dodge EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dodge :: EventCard Dodge
dodge = event Dodge Cards.dodge

instance RunMessage Dodge where
  runMessage msg e@(Dodge attrs) = runQueueT $ case msg of
    PlayThisEvent _ (is attrs -> True) -> do
      cancelAttack attrs (getAttackDetails attrs.windows)
      pure e 
    _ -> Dodge <$> liftRunMessage msg attrs
