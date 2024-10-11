module Arkham.Event.Events.Eucatastrophe3 (eucatastrophe3, Eucatastrophe3 (..)) where

import Arkham.ChaosToken
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Window (getChaosToken)
import Arkham.Modifier
import Arkham.Strategy
import Arkham.Taboo

newtype Eucatastrophe3 = Eucatastrophe3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eucatastrophe3 :: EventCard Eucatastrophe3
eucatastrophe3 =
  eventWith Eucatastrophe3 Cards.eucatastrophe3
    $ \a -> if tabooed TabooList19 a then a {eventAfterPlay = RemoveThisFromGame} else a

instance RunMessage Eucatastrophe3 where
  runMessage msg e@(Eucatastrophe3 attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent _ eid _ (getChaosToken -> token) _ | eid == toId attrs -> do
      chaosTokenEffect attrs token $ ChaosTokenFaceModifier [ElderSign]
      pure e
    _ -> Eucatastrophe3 <$> liftRunMessage msg attrs
