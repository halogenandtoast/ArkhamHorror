module Arkham.Types.Event.Cards.MomentOfRespite3
  ( momentOfRespite3
  , MomentOfRespite3(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Message
import Arkham.Types.Target

newtype MomentOfRespite3 = MomentOfRespite3 EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

momentOfRespite3 :: EventCard MomentOfRespite3
momentOfRespite3 = event MomentOfRespite3 Cards.momentOfRespite3

instance HasActions MomentOfRespite3
instance HasModifiersFor env MomentOfRespite3

instance RunMessage env MomentOfRespite3 where
  runMessage msg e@(MomentOfRespite3 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == toId attrs -> do
      e <$ pushAll
        [ HealHorror (InvestigatorTarget iid) 3
        , DrawCards iid 1 False
        , Discard (toTarget attrs)
        ]
    _ -> MomentOfRespite3 <$> runMessage msg attrs
