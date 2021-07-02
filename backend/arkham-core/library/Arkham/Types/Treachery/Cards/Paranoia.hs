module Arkham.Types.Treachery.Cards.Paranoia where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype Paranoia = Paranoia TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

paranoia :: TreacheryCard Paranoia
paranoia = treachery Paranoia Cards.paranoia

instance HasModifiersFor env Paranoia where
  getModifiersFor = noModifiersFor

instance HasActions env Paranoia where
  getActions i window (Paranoia attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env Paranoia where
  runMessage msg t@(Paranoia attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      resourceCount' <- unResourceCount <$> getCount iid
      t <$ unshiftMessages
        [SpendResources iid resourceCount', Discard $ toTarget attrs]
    _ -> Paranoia <$> runMessage msg attrs
