{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.Amnesia where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

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
      cardCount' <- unCardCount <$> getCount iid
      unshiftMessages $ replicate (cardCount' - 1) (ChooseAndDiscardCard iid)
      Amnesia <$> runMessage msg (attrs & resolved .~ True)
    _ -> Amnesia <$> runMessage msg attrs
