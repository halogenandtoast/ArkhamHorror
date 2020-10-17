{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.WillToSurvive4 where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target
import Lens.Micro

import ClassyPrelude

newtype WillToSurvive4 = WillToSurvive4 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

willToSurvive4 :: InvestigatorId -> EventId -> WillToSurvive4
willToSurvive4 iid uuid = WillToSurvive4 $ baseAttrs iid uuid "01085"

instance HasActions env WillToSurvive4 where
  getActions i window (WillToSurvive4 attrs) = getActions i window attrs

instance HasQueue env => RunMessage env WillToSurvive4 where
  runMessage msg (WillToSurvive4 attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      unshiftMessages
        [ AddModifiers
          (InvestigatorTarget iid)
          (EndOfTurnSource (EventSource eid))
          [DoNotDrawChaosTokensForSkillChecks]
        , Discard (EventTarget eid)
        ]
      WillToSurvive4 <$> runMessage msg (attrs & resolved .~ True)
    _ -> WillToSurvive4 <$> runMessage msg attrs
