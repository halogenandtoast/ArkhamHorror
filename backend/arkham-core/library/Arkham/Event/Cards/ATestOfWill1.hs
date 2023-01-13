module Arkham.Event.Cards.ATestOfWill1
  ( aTestOfWill1
  , ATestOfWill1(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Message

newtype ATestOfWill1 = ATestOfWill1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aTestOfWill1 :: EventCard ATestOfWill1
aTestOfWill1 = event ATestOfWill1 Cards.aTestOfWill1

instance RunMessage ATestOfWill1 where
  runMessage msg e@(ATestOfWill1 attrs) = case msg of
    InvestigatorPlayEvent _ eid _ _ _ | eid == toId attrs ->
      e <$ pushAll [CancelNext (toSource attrs) RevelationMessage, Exile $ toTarget attrs]
    _ -> ATestOfWill1 <$> runMessage msg attrs
