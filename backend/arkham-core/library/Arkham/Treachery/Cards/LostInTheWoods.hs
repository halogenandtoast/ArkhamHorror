module Arkham.Treachery.Cards.LostInTheWoods
  ( lostInTheWoods
  , LostInTheWoods(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype LostInTheWoods = LostInTheWoods TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostInTheWoods :: TreacheryCard LostInTheWoods
lostInTheWoods = treachery LostInTheWoods Cards.lostInTheWoods

instance RunMessage LostInTheWoods where
  runMessage msg t@(LostInTheWoods attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> LostInTheWoods <$> runMessage msg attrs
