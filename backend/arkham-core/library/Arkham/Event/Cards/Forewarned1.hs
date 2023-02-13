module Arkham.Event.Cards.Forewarned1
  ( forewarned1
  , Forewarned1(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Message

newtype Forewarned1 = Forewarned1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forewarned1 :: EventCard Forewarned1
forewarned1 = event Forewarned1 Cards.forewarned1

instance RunMessage Forewarned1 where
  runMessage msg e@(Forewarned1 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      e <$ pushAll
        [ InvestigatorPlaceCluesOnLocation iid 1
        , CancelNext (toSource attrs) RevelationMessage
        ]
    _ -> Forewarned1 <$> runMessage msg attrs
