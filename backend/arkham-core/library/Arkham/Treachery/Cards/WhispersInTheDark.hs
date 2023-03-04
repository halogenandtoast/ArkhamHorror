module Arkham.Treachery.Cards.WhispersInTheDark
  ( whispersInTheDark
  , WhispersInTheDark(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Runner

newtype WhispersInTheDark = WhispersInTheDark TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whispersInTheDark :: TreacheryCard WhispersInTheDark
whispersInTheDark = treachery WhispersInTheDark Cards.whispersInTheDark

instance RunMessage WhispersInTheDark where
  runMessage msg t@(WhispersInTheDark attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> WhispersInTheDark <$> runMessage msg attrs
