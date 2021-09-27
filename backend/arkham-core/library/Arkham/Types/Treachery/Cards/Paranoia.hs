module Arkham.Types.Treachery.Cards.Paranoia where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype Paranoia = Paranoia TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

paranoia :: TreacheryCard Paranoia
paranoia = treachery Paranoia Cards.paranoia

instance TreacheryRunner env => RunMessage env Paranoia where
  runMessage msg t@(Paranoia attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      resourceCount' <- unResourceCount <$> getCount iid
      t <$ push (SpendResources iid resourceCount')
    _ -> Paranoia <$> runMessage msg attrs
