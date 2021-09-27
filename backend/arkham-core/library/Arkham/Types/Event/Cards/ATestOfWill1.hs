module Arkham.Types.Event.Cards.ATestOfWill1
  ( aTestOfWill1
  , ATestOfWill1(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Message

newtype ATestOfWill1 = ATestOfWill1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aTestOfWill1 :: EventCard ATestOfWill1
aTestOfWill1 = event ATestOfWill1 Cards.aTestOfWill1

instance EventRunner env => RunMessage env ATestOfWill1 where
  runMessage msg e@(ATestOfWill1 attrs) = case msg of
    InvestigatorPlayEvent _ eid _ _ | eid == toId attrs ->
      e <$ pushAll [CancelNext RevelationMessage, Exile $ toTarget attrs]
    _ -> ATestOfWill1 <$> runMessage msg attrs
