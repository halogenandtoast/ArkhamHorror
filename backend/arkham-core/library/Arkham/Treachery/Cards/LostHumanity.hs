module Arkham.Treachery.Cards.LostHumanity
  ( lostHumanity
  , LostHumanity(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype LostHumanity = LostHumanity TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostHumanity :: TreacheryCard LostHumanity
lostHumanity = treachery LostHumanity Cards.lostHumanity

instance RunMessage LostHumanity where
  runMessage msg t@(LostHumanity attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> LostHumanity <$> runMessage msg attrs
