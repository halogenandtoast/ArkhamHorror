{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.OnTheLam where

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

newtype OnTheLam = OnTheLam Attrs
  deriving newtype (Show, ToJSON, FromJSON)

onTheLam :: InvestigatorId -> EventId -> OnTheLam
onTheLam iid uuid = OnTheLam $ baseAttrs iid uuid "01010"

instance HasActions env investigator OnTheLam where
  getActions i window (OnTheLam attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env OnTheLam where
  runMessage msg (OnTheLam attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid | eid == eventId -> do
      unshiftMessages
        [ AddModifiers
          (InvestigatorTarget iid)
          (EventSource eid)
          [CannotBeAttackedByNonElite]
        , Discard (EventTarget eid)
        ]
      OnTheLam <$> runMessage msg (attrs & resolved .~ True)
    _ -> OnTheLam <$> runMessage msg attrs
