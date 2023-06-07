module Arkham.Treachery.Cards.MarkOfTheOrder
  ( markOfTheOrder
  , MarkOfTheOrder(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype MarkOfTheOrder = MarkOfTheOrder TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

markOfTheOrder :: TreacheryCard MarkOfTheOrder
markOfTheOrder = treachery MarkOfTheOrder Cards.markOfTheOrder

instance RunMessage MarkOfTheOrder where
  runMessage msg t@(MarkOfTheOrder attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> MarkOfTheOrder <$> runMessage msg attrs
