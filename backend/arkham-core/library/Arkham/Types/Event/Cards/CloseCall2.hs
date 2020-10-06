{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.CloseCall2 where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Target
import Lens.Micro

import ClassyPrelude

newtype CloseCall2 = CloseCall2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

closeCall2 :: InvestigatorId -> EventId -> CloseCall2
closeCall2 iid uuid = CloseCall2 $ baseAttrs iid uuid "01083"

instance HasActions env investigator CloseCall2 where
  getActions i window (CloseCall2 attrs) = getActions i window attrs

instance HasQueue env => RunMessage env CloseCall2 where
  runMessage msg (CloseCall2 attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent _iid eid (Just (EnemyTarget enemyId)) | eid == eventId -> do
      unshiftMessage $ ShuffleBackIntoEncounterDeck (EnemyTarget enemyId)
      CloseCall2 <$> runMessage msg (attrs & resolved .~ True)
    _ -> CloseCall2 <$> runMessage msg attrs
