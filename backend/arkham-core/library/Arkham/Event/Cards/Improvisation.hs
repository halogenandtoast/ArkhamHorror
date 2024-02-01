module Arkham.Event.Cards.Improvisation (
  improvisation,
  Improvisation (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Id

newtype Improvisation = Improvisation EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

improvisation :: EventCard Improvisation
improvisation = event Improvisation Cards.improvisation

switchRole :: PlayerId -> InvestigatorId -> Message
switchRole pid iid =
  chooseOne
    pid
    [Label (tshow role) [SetRole iid role] | role <- [minBound .. maxBound]]

reductionEffect :: InvestigatorId -> EventAttrs -> Message
reductionEffect iid attrs =
  CreateEffect "03018" Nothing (toSource attrs) (InvestigatorTarget iid)

instance RunMessage Improvisation where
  runMessage msg e@(Improvisation attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      drawing <- drawCards iid attrs 1
      player <- getPlayer iid
      pushAll
        [ switchRole player iid
        , reductionEffect iid attrs
        , drawing
        ]
      pure e
    _ -> Improvisation <$> runMessage msg attrs
