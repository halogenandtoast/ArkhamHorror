module Arkham.Treachery.Cards.BaneOfTheLiving
  ( baneOfTheLiving
  , BaneOfTheLiving(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype BaneOfTheLiving = BaneOfTheLiving TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baneOfTheLiving :: TreacheryCard BaneOfTheLiving
baneOfTheLiving = treachery BaneOfTheLiving Cards.baneOfTheLiving

instance RunMessage BaneOfTheLiving where
  runMessage msg t@(BaneOfTheLiving attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> BaneOfTheLiving <$> runMessage msg attrs
