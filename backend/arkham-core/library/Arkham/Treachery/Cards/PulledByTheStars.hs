module Arkham.Treachery.Cards.PulledByTheStars
  ( pulledByTheStars
  , PulledByTheStars(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype PulledByTheStars = PulledByTheStars TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pulledByTheStars :: TreacheryCard PulledByTheStars
pulledByTheStars = treachery PulledByTheStars Cards.pulledByTheStars

instance RunMessage PulledByTheStars where
  runMessage msg t@(PulledByTheStars attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> PulledByTheStars <$> runMessage msg attrs
