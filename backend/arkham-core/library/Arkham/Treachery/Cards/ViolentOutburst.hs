module Arkham.Treachery.Cards.ViolentOutburst
  ( violentOutburst
  , ViolentOutburst(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ViolentOutburst = ViolentOutburst TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

violentOutburst :: TreacheryCard ViolentOutburst
violentOutburst = treachery ViolentOutburst Cards.violentOutburst

instance RunMessage ViolentOutburst where
  runMessage msg t@(ViolentOutburst attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> ViolentOutburst <$> runMessage msg attrs
