module Arkham.Treachery.Cards.LostInTheWilds
  ( lostInTheWilds
  , LostInTheWilds(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Runner

newtype LostInTheWilds = LostInTheWilds TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostInTheWilds :: TreacheryCard LostInTheWilds
lostInTheWilds = treachery LostInTheWilds Cards.lostInTheWilds

instance RunMessage LostInTheWilds where
  runMessage msg t@(LostInTheWilds attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> LostInTheWilds <$> runMessage msg attrs
