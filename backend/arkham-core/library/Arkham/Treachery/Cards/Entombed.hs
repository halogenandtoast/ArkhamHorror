module Arkham.Treachery.Cards.Entombed
  ( entombed
  , Entombed(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Runner

newtype Entombed = Entombed TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

entombed :: TreacheryCard Entombed
entombed = treachery Entombed Cards.entombed

instance RunMessage Entombed where
  runMessage msg t@(Entombed attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> Entombed <$> runMessage msg attrs
