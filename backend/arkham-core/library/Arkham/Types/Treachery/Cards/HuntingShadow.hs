{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.HuntingShadow where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Source
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import ClassyPrelude

newtype HuntingShadow = HuntingShadow Attrs
  deriving newtype (Show, ToJSON, FromJSON)

huntingShadow :: TreacheryId -> HuntingShadow
huntingShadow uuid = HuntingShadow $ baseAttrs uuid "01135"

instance (TreacheryRunner env) => RunMessage env HuntingShadow where
  runMessage msg (HuntingShadow attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      clueCount <- unClueCount <$> asks (getCount iid)
      if clueCount > 0
        then unshiftMessage
          (Ask iid $ ChooseOne
            [ SpendClues 1 [iid]
            , InvestigatorAssignDamage iid (TreacherySource tid) 2 0
            ]
          )
        else unshiftMessage
          (InvestigatorAssignDamage iid (TreacherySource tid) 2 0)
      HuntingShadow <$> runMessage msg attrs
    _ -> HuntingShadow <$> runMessage msg attrs
