module Arkham.Treachery.Cards.DaemonicPiping (
  daemonicPiping,
  DaemonicPiping (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype DaemonicPiping = DaemonicPiping TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daemonicPiping :: TreacheryCard DaemonicPiping
daemonicPiping = treachery DaemonicPiping Cards.daemonicPiping

instance RunMessage DaemonicPiping where
  runMessage msg t@(DaemonicPiping attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> DaemonicPiping <$> runMessage msg attrs
