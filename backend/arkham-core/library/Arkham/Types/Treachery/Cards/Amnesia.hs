{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.Amnesia where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import ClassyPrelude
import Lens.Micro

newtype Amnesia = Amnesia Attrs
  deriving newtype (Show, ToJSON, FromJSON)

amnesia :: TreacheryId -> Amnesia
amnesia uuid = Amnesia $ weaknessAttrs uuid "01096"

instance HasActions env investigator Amnesia where
  getActions i window (Amnesia attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env Amnesia where
  runMessage msg (Amnesia attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      cardCount' <- unCardCount <$> asks (getCount iid)
      unshiftMessages $ replicate (cardCount' - 1) (ChooseAndDiscardCard iid)
      Amnesia <$> runMessage msg (attrs & resolved .~ True)
    _ -> Amnesia <$> runMessage msg attrs
