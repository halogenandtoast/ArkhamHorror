{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.Paranoia where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import ClassyPrelude
import Lens.Micro

newtype Paranoia = Paranoia Attrs
  deriving newtype (Show, ToJSON, FromJSON)

paranoia :: TreacheryId -> Paranoia
paranoia uuid = Paranoia $ weaknessAttrs uuid "01097"

instance HasActions env investigator Paranoia where
  getActions i window (Paranoia attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env Paranoia where
  runMessage msg (Paranoia attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      resourceCount' <- unResourceCount <$> asks (getCount iid)
      unshiftMessage (SpendResources iid resourceCount')
      Paranoia <$> runMessage msg (attrs & resolved .~ True)
    _ -> Paranoia <$> runMessage msg attrs
