{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.WorkingAHunch where

import Arkham.Import

import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype WorkingAHunch = WorkingAHunch Attrs
  deriving newtype (Show, ToJSON, FromJSON)

workingAHunch :: InvestigatorId -> EventId -> WorkingAHunch
workingAHunch iid uuid = WorkingAHunch $ baseAttrs iid uuid "01037"

instance HasModifiersFor env WorkingAHunch where
  getModifiersFor = noModifiersFor

instance HasActions env WorkingAHunch where
  getActions i window (WorkingAHunch attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env WorkingAHunch where
  runMessage msg (WorkingAHunch attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      currentLocationId <- asks (getId @LocationId iid)
      locationClueCount <- unClueCount <$> asks (getCount currentLocationId)
      if locationClueCount > 0
        then unshiftMessages
          [ DiscoverCluesAtLocation iid currentLocationId 1
          , Discard (EventTarget eid)
          ]
        else unshiftMessages [Discard (EventTarget eid)]
      WorkingAHunch <$> runMessage msg (attrs & resolved .~ True)
    _ -> WorkingAHunch <$> runMessage msg attrs
