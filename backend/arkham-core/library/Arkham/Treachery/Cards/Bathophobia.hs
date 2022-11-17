module Arkham.Treachery.Cards.Bathophobia
  ( bathophobia
  , Bathophobia(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Bathophobia = Bathophobia TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bathophobia :: TreacheryCard Bathophobia
bathophobia = treachery Bathophobia Cards.bathophobia

instance RunMessage Bathophobia where
  runMessage msg t@(Bathophobia attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> Bathophobia <$> runMessage msg attrs
