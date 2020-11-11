{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.HotStreak2 where

import Arkham.Import

import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype HotStreak2 = HotStreak2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

hotStreak2 :: InvestigatorId -> EventId -> HotStreak2
hotStreak2 iid uuid = HotStreak2 $ baseAttrs iid uuid "50006"

instance HasModifiersFor env HotStreak2 where
  getModifiersFor = noModifiersFor

instance HasActions env HotStreak2 where
  getActions i window (HotStreak2 attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env HotStreak2 where
  runMessage msg (HotStreak2 attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      unshiftMessages [TakeResources iid 10 False, Discard (EventTarget eid)]
      HotStreak2 <$> runMessage msg (attrs & resolved .~ True)
    _ -> HotStreak2 <$> runMessage msg attrs
