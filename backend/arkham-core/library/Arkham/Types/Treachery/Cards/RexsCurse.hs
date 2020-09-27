{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.RexsCurse where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import ClassyPrelude
import Lens.Micro

newtype RexsCurse = RexsCurse Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

rexsCurse :: TreacheryId -> Maybe InvestigatorId -> RexsCurse
rexsCurse uuid iid = RexsCurse $ weaknessAttrs uuid iid "02009"

instance HasActions env investigator RexsCurse where
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env RexsCurse where
  runMessage msg (RexsCurse attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      unshiftMessage (AttachTreachery treacheryId (InvestigatorTarget iid))
      RexsCurse <$> runMessage msg (attrs & attachedInvestigator ?~ iid)
    _ -> RexsCurse <$> runMessage msg attrs
