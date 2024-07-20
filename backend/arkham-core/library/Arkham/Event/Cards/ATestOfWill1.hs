module Arkham.Event.Cards.ATestOfWill1 (aTestOfWill1, ATestOfWill1 (..)) where

import Arkham.Capability
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Id
import Arkham.Matcher hiding (DrawCard)
import Arkham.Window

newtype ATestOfWill1 = ATestOfWill1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aTestOfWill1 :: EventCard ATestOfWill1
aTestOfWill1 = event ATestOfWill1 Cards.aTestOfWill1

getDetails :: [Window] -> (InvestigatorId, Card)
getDetails ((windowType -> DrawCard who card _) : _) = (who, card)
getDetails (_ : rest) = getDetails rest
getDetails [] = error "missing targets"

instance RunMessage ATestOfWill1 where
  runMessage msg e@(ATestOfWill1 attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent iid eid _ (getDetails -> (who, card)) _ | eid == toId attrs -> do
      cancelRevelation attrs card
      push $ Exile $ toTarget attrs
      pure e
    _ -> ATestOfWill1 <$> liftRunMessage msg attrs
