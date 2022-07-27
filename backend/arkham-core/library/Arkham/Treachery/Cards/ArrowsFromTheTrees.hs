module Arkham.Treachery.Cards.ArrowsFromTheTrees
  ( arrowsFromTheTrees
  , ArrowsFromTheTrees(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Runner

newtype ArrowsFromTheTrees = ArrowsFromTheTrees TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arrowsFromTheTrees :: TreacheryCard ArrowsFromTheTrees
arrowsFromTheTrees = treachery ArrowsFromTheTrees Cards.arrowsFromTheTrees

instance RunMessage ArrowsFromTheTrees where
  runMessage msg t@(ArrowsFromTheTrees attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> ArrowsFromTheTrees <$> runMessage msg attrs
