{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.OnTheLam where

import Arkham.Import

import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype OnTheLam = OnTheLam Attrs
  deriving newtype (Show, ToJSON, FromJSON)

onTheLam :: InvestigatorId -> EventId -> OnTheLam
onTheLam iid uuid = OnTheLam $ baseAttrs iid uuid "01010"

instance HasModifiersFor env OnTheLam where
  getModifiersFor = noModifiersFor

instance HasActions env OnTheLam where
  getActions i window (OnTheLam attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env OnTheLam where
  runMessage msg e@(OnTheLam attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      e <$ unshiftEffect attrs (InvestigatorTarget iid)
    _ -> OnTheLam <$> runMessage msg attrs
