module Arkham.Types.Event.Cards.Forewarned1
  ( forewarned1
  , Forewarned1(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Message

newtype Forewarned1 = Forewarned1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forewarned1 :: EventCard Forewarned1
forewarned1 = event Forewarned1 Cards.forewarned1

instance EventRunner env => RunMessage env Forewarned1 where
  runMessage msg e@(Forewarned1 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == toId attrs -> do
      e <$ pushAll
        [ InvestigatorPlaceCluesOnLocation iid 1
        , CancelNext RevelationMessage
        , Discard (toTarget attrs)
        ]
    _ -> Forewarned1 <$> runMessage msg attrs
