module Arkham.Treachery.Cards.CaptiveMind
  ( captiveMind
  , CaptiveMind(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype CaptiveMind = CaptiveMind TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

captiveMind :: TreacheryCard CaptiveMind
captiveMind = treachery CaptiveMind Cards.captiveMind

instance RunMessage CaptiveMind where
  runMessage msg t@(CaptiveMind attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> CaptiveMind <$> runMessage msg attrs
