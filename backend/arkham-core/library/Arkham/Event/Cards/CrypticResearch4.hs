module Arkham.Event.Cards.CrypticResearch4 where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Event.Runner
import Arkham.Id
import Arkham.Message
import Arkham.Target

newtype CrypticResearch4 = CrypticResearch4 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crypticResearch4 :: EventCard CrypticResearch4
crypticResearch4 = event CrypticResearch4 Cards.crypticResearch4

instance (EventRunner env) => RunMessage CrypticResearch4 where
  runMessage msg e@(CrypticResearch4 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      locationId <- getId @LocationId iid
      investigatorIds <- getSetList locationId
      e <$ pushAll
        [ chooseOne
          iid
          [ TargetLabel (InvestigatorTarget iid') [DrawCards iid' 3 False]
          | iid' <- investigatorIds
          ]
        , Discard (EventTarget eid)
        ]
    _ -> CrypticResearch4 <$> runMessage msg attrs
