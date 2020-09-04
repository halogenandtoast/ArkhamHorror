{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.HotStreak2 where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Target
import Lens.Micro

import ClassyPrelude

newtype HotStreak2 = HotStreak2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

hotStreak2 :: InvestigatorId -> EventId -> HotStreak2
hotStreak2 iid uuid = HotStreak2 $ baseAttrs iid uuid "50006"

instance HasActions env investigator HotStreak2 where
  getActions i window (HotStreak2 attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env HotStreak2 where
  runMessage msg (HotStreak2 attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid | eid == eventId -> do
      unshiftMessages [TakeResources iid 10 False, Discard (EventTarget eid)]
      HotStreak2 <$> runMessage msg (attrs & resolved .~ True)
    _ -> HotStreak2 <$> runMessage msg attrs
