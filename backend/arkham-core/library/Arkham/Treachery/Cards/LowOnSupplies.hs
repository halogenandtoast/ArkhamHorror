module Arkham.Treachery.Cards.LowOnSupplies
  ( lowOnSupplies
  , LowOnSupplies(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Runner

newtype LowOnSupplies = LowOnSupplies TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lowOnSupplies :: TreacheryCard LowOnSupplies
lowOnSupplies = treachery LowOnSupplies Cards.lowOnSupplies

instance RunMessage LowOnSupplies where
  runMessage msg t@(LowOnSupplies attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> LowOnSupplies <$> runMessage msg attrs
