module Arkham.Treachery.Cards.SerpentsIre
  ( serpentsIre
  , SerpentsIre(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype SerpentsIre = SerpentsIre TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

serpentsIre :: TreacheryCard SerpentsIre
serpentsIre = treachery SerpentsIre Cards.serpentsIre

instance RunMessage SerpentsIre where
  runMessage msg t@(SerpentsIre attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> SerpentsIre <$> runMessage msg attrs
