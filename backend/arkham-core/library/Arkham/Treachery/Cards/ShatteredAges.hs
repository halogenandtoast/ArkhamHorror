module Arkham.Treachery.Cards.ShatteredAges
  ( shatteredAges
  , ShatteredAges(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ShatteredAges = ShatteredAges TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shatteredAges :: TreacheryCard ShatteredAges
shatteredAges = treachery ShatteredAges Cards.shatteredAges

instance RunMessage ShatteredAges where
  runMessage msg t@(ShatteredAges attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> ShatteredAges <$> runMessage msg attrs
