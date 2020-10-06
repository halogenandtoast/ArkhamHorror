{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.Lucky2 where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target
import Lens.Micro

import ClassyPrelude

newtype Lucky2 = Lucky2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

lucky2 :: InvestigatorId -> EventId -> Lucky2
lucky2 iid uuid = Lucky2 $ baseAttrs iid uuid "01084"

instance HasActions env investigator Lucky2 where
  getActions i window (Lucky2 attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env Lucky2 where
  runMessage msg (Lucky2 attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      unshiftMessages
        [ AddModifiers AfterSkillTestTarget (EventSource eid) [AnySkillValue 2]
        , DrawCards iid 1 False
        , Discard (EventTarget eid)
        ]
      Lucky2 <$> runMessage msg (attrs & resolved .~ True)
    _ -> Lucky2 <$> runMessage msg attrs
