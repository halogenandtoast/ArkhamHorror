module Arkham.Treachery.Cards.Pitfall
  ( pitfall
  , Pitfall(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Runner

newtype Pitfall = Pitfall TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pitfall :: TreacheryCard Pitfall
pitfall = treachery Pitfall Cards.pitfall

instance RunMessage Pitfall where
  runMessage msg t@(Pitfall attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> Pitfall <$> runMessage msg attrs
