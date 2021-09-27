module Arkham.Types.Event.Cards.FightOrFlight
  ( fightOrFlight
  , FightOrFlight(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Message
import Arkham.Types.Target

newtype FightOrFlight = FightOrFlight EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fightOrFlight :: EventCard FightOrFlight
fightOrFlight = event FightOrFlight Cards.fightOrFlight

instance EventRunner env => RunMessage env FightOrFlight where
  runMessage msg e@(FightOrFlight attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == toId attrs -> do
      e <$ pushAll
        [ CreateEffect
          (toCardCode attrs)
          Nothing
          (toSource attrs)
          (InvestigatorTarget iid)
        , Discard (toTarget attrs)
        ]
    _ -> FightOrFlight <$> runMessage msg attrs
