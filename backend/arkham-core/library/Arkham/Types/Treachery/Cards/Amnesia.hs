module Arkham.Types.Treachery.Cards.Amnesia where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.TreacheryId


import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype Amnesia = Amnesia TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

amnesia :: TreacheryId -> Maybe InvestigatorId -> Amnesia
amnesia uuid iid = Amnesia $ weaknessAttrs uuid iid "01096"

instance HasModifiersFor env Amnesia where
  getModifiersFor = noModifiersFor

instance HasActions env Amnesia where
  getActions i window (Amnesia attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env Amnesia where
  runMessage msg t@(Amnesia attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      cardCount' <- unCardCount <$> getCount iid
      t <$ unshiftMessages
        (replicate (cardCount' - 1) (ChooseAndDiscardCard iid)
        <> [Discard $ toTarget attrs]
        )
    _ -> Amnesia <$> runMessage msg attrs
