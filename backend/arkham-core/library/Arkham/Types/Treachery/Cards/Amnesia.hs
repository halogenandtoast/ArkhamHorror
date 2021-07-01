module Arkham.Types.Treachery.Cards.Amnesia where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype Amnesia = Amnesia TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

amnesia :: TreacheryCard Amnesia
amnesia = treachery Amnesia Cards.amnesia

instance HasModifiersFor env Amnesia where
  getModifiersFor = noModifiersFor

instance HasActions env Amnesia where
  getActions i window (Amnesia attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env Amnesia where
  runMessage msg t@(Amnesia attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      cardCount' <- unCardCount <$> getCount iid
      t <$ unshiftMessages
        (replicate (cardCount' - 1) (ChooseAndDiscardCard iid)
        <> [Discard $ toTarget attrs]
        )
    _ -> Amnesia <$> runMessage msg attrs
