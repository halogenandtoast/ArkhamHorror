module Arkham.Event.Cards.Improvisation
  ( improvisation
  , Improvisation(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Id
import Arkham.Message
import Arkham.Target

newtype Improvisation = Improvisation EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

improvisation :: EventCard Improvisation
improvisation = event Improvisation Cards.improvisation

switchRole :: InvestigatorId -> Message
switchRole iid = chooseOne
  iid
  [ Label (tshow role) [SetRole iid role] | role <- [minBound .. maxBound] ]

reductionEffect :: InvestigatorId -> EventAttrs -> Message
reductionEffect iid attrs =
  CreateEffect "03018" Nothing (toSource attrs) (InvestigatorTarget iid)

instance RunMessage Improvisation where
  runMessage msg e@(Improvisation attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      drawing <- drawCards iid attrs 1
      pushAll
        [ switchRole iid
        , reductionEffect iid attrs
        , drawing
        , discard attrs
        ]
      pure e
    _ -> Improvisation <$> runMessage msg attrs
