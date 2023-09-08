module Arkham.Treachery.Cards.TheEndIsNigh
  ( theEndIsNigh
  , TheEndIsNigh(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype TheEndIsNigh = TheEndIsNigh TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEndIsNigh :: TreacheryCard TheEndIsNigh
theEndIsNigh = treachery TheEndIsNigh Cards.theEndIsNigh

instance RunMessage TheEndIsNigh where
  runMessage msg t@(TheEndIsNigh attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> TheEndIsNigh <$> runMessage msg attrs
