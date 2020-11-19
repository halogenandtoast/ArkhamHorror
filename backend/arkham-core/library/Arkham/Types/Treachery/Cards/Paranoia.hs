{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.Paranoia where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import ClassyPrelude
import Lens.Micro

newtype Paranoia = Paranoia Attrs
  deriving newtype (Show, ToJSON, FromJSON)

paranoia :: TreacheryId -> Maybe InvestigatorId -> Paranoia
paranoia uuid iid = Paranoia $ weaknessAttrs uuid iid "01097"

instance HasModifiersFor env Paranoia where
  getModifiersFor = noModifiersFor

instance HasActions env Paranoia where
  getActions i window (Paranoia attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env Paranoia where
  runMessage msg (Paranoia attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      resourceCount' <- unResourceCount <$> getCount iid
      unshiftMessage (SpendResources iid resourceCount')
      Paranoia <$> runMessage msg (attrs & resolved .~ True)
    _ -> Paranoia <$> runMessage msg attrs
