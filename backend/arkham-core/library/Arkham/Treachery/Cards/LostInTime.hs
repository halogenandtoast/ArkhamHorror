module Arkham.Treachery.Cards.LostInTime
  ( lostInTime
  , LostInTime(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Runner

newtype LostInTime = LostInTime TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostInTime :: TreacheryCard LostInTime
lostInTime = treachery LostInTime Cards.lostInTime

instance RunMessage LostInTime where
  runMessage msg t@(LostInTime attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> LostInTime <$> runMessage msg attrs
