module Arkham.Types.Event.Cards.Improvisation
  ( improvisation
  , Improvisation(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target

newtype Improvisation = Improvisation EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

improvisation :: EventCard Improvisation
improvisation = event Improvisation Cards.improvisation

instance HasActions Improvisation
instance HasModifiersFor env Improvisation

switchRole :: InvestigatorId -> Message
switchRole iid = chooseOne
  iid
  [ Label (tshow role) [SetRole iid role] | role <- [minBound .. maxBound] ]

reductionEffect :: InvestigatorId -> EventAttrs -> Message
reductionEffect iid attrs =
  CreateEffect "03018" Nothing (toSource attrs) (InvestigatorTarget iid)

instance RunMessage env Improvisation where
  runMessage msg e@(Improvisation attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == toId attrs -> do
      e <$ pushAll
        [ switchRole iid
        , reductionEffect iid attrs
        , DrawCards iid 1 False
        , Discard (toTarget attrs)
        ]
    _ -> Improvisation <$> runMessage msg attrs
