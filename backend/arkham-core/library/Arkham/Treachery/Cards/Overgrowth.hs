module Arkham.Treachery.Cards.Overgrowth
  ( overgrowth
  , Overgrowth(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Runner

newtype Overgrowth = Overgrowth TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overgrowth :: TreacheryCard Overgrowth
overgrowth = treachery Overgrowth Cards.overgrowth

instance RunMessage Overgrowth where
  runMessage msg t@(Overgrowth attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> Overgrowth <$> runMessage msg attrs
