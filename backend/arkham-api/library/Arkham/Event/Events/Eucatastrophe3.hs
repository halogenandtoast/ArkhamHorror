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
eucatastrophe3 = event Eucatastrophe3 Cards.eucatastrophe3

instance RunMessage Eucatastrophe3 where
  runMessage msg e@(Eucatastrophe3 attrs) = runQueueT $ case msg of
    CardEnteredPlay _ card | attrs.cardId == card.id -> do
      attrs' <- liftRunMessage msg attrs
      pure
        $ Eucatastrophe3
        $ attrs'
        & if tabooed TabooList19 attrs' then afterPlayL .~ RemoveThisFromGame else id
    InvestigatorPlayEvent _ eid _ (getChaosToken -> token) _ | eid == toId attrs -> do
      chaosTokenEffect attrs token $ ChaosTokenFaceModifier [ElderSign]
      pure e
    _ -> Eucatastrophe3 <$> liftRunMessage msg attrs
