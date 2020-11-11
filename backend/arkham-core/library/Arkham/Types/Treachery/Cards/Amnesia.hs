{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.Amnesia where

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

newtype Amnesia = Amnesia Attrs
  deriving newtype (Show, ToJSON, FromJSON)

amnesia :: TreacheryId -> Maybe InvestigatorId -> Amnesia
amnesia uuid iid = Amnesia $ weaknessAttrs uuid iid "01096"

instance HasModifiersFor env Amnesia where
  getModifiersFor = noModifiersFor

instance HasActions env Amnesia where
  getActions i window (Amnesia attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env Amnesia where
  runMessage msg (Amnesia attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      cardCount' <- unCardCount <$> asks (getCount iid)
      unshiftMessages $ replicate (cardCount' - 1) (ChooseAndDiscardCard iid)
      Amnesia <$> runMessage msg (attrs & resolved .~ True)
    _ -> Amnesia <$> runMessage msg attrs
