module Arkham.Treachery.Cards.ChildrenOfValusia
  ( childrenOfValusia
  , ChildrenOfValusia(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ChildrenOfValusia = ChildrenOfValusia TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

childrenOfValusia :: TreacheryCard ChildrenOfValusia
childrenOfValusia = treachery ChildrenOfValusia Cards.childrenOfValusia

instance RunMessage ChildrenOfValusia where
  runMessage msg t@(ChildrenOfValusia attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> ChildrenOfValusia <$> runMessage msg attrs
