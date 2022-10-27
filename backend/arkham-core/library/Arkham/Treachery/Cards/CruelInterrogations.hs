module Arkham.Treachery.Cards.CruelInterrogations
  ( cruelInterrogations
  , CruelInterrogations(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype CruelInterrogations = CruelInterrogations TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cruelInterrogations :: TreacheryCard CruelInterrogations
cruelInterrogations = treachery CruelInterrogations Cards.cruelInterrogations

instance RunMessage CruelInterrogations where
  runMessage msg t@(CruelInterrogations attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> CruelInterrogations <$> runMessage msg attrs
