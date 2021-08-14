module Arkham.Types.Event.Cards.CrypticResearch4 where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target

newtype CrypticResearch4 = CrypticResearch4 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crypticResearch4 :: EventCard CrypticResearch4
crypticResearch4 = event CrypticResearch4 Cards.crypticResearch4

instance (EventRunner env) => RunMessage env CrypticResearch4 where
  runMessage msg e@(CrypticResearch4 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == eventId -> do
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
