module Arkham.Treachery.Cards.Expulsion
  ( expulsion
  , Expulsion(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Expulsion = Expulsion TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

expulsion :: TreacheryCard Expulsion
expulsion = treachery Expulsion Cards.expulsion

instance RunMessage Expulsion where
  runMessage msg t@(Expulsion attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Expulsion <$> runMessage msg attrs
