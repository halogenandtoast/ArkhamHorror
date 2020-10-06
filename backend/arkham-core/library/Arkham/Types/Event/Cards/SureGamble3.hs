{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.SureGamble3 where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Token
import Lens.Micro

import ClassyPrelude

newtype SureGamble3 = SureGamble3 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

sureGamble3 :: InvestigatorId -> EventId -> SureGamble3
sureGamble3 iid uuid = SureGamble3 $ baseAttrs iid uuid "01088"

instance HasActions env investigator SureGamble3 where
  getActions i window (SureGamble3 attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env SureGamble3 where
  runMessage msg (SureGamble3 attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      mrunSkillTest <- popMessage
      case mrunSkillTest of
        Just (RunSkillTest _ (TokenValue token tokenValue)) ->
          unshiftMessage (RunSkillTest iid (TokenValue token (-tokenValue)))
        _ -> error "We expected this run skill test to be next"
      SureGamble3 <$> runMessage msg (attrs & resolved .~ True)
    _ -> SureGamble3 <$> runMessage msg attrs
