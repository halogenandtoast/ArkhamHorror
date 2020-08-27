{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.AbandonedAndAlone where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import ClassyPrelude
import Lens.Micro

newtype AbandonedAndAlone = AbandonedAndAlone Attrs
  deriving newtype (Show, ToJSON, FromJSON)

abandonedAndAlone :: TreacheryId -> Maybe InvestigatorId -> AbandonedAndAlone
abandonedAndAlone uuid iid = AbandonedAndAlone $ weaknessAttrs uuid iid "01015"

instance HasActions env investigator AbandonedAndAlone where
  getActions i window (AbandonedAndAlone attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env AbandonedAndAlone where
  runMessage msg (AbandonedAndAlone attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      unshiftMessages
        [ InvestigatorDirectDamage iid (TreacherySource treacheryId) 0 2
        , RemoveDiscardFromGame iid
        ]
      AbandonedAndAlone <$> runMessage msg (attrs & resolved .~ True)
    _ -> AbandonedAndAlone <$> runMessage msg attrs
