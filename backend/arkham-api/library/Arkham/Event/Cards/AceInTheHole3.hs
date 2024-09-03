module Arkham.Event.Cards.AceInTheHole3 (aceInTheHole3, AceInTheHole3 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted

newtype AceInTheHole3 = AceInTheHole3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aceInTheHole3 :: EventCard AceInTheHole3
aceInTheHole3 = event AceInTheHole3 Cards.aceInTheHole3

instance RunMessage AceInTheHole3 where
  runMessage msg e@(AceInTheHole3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == attrs.id -> do
      gainActions iid attrs 3
      pure e
    _ -> AceInTheHole3 <$> liftRunMessage msg attrs
