module Arkham.Treachery.Cards.CloseWatch (
  closeWatch,
  CloseWatch (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype CloseWatch = CloseWatch TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

closeWatch :: TreacheryCard CloseWatch
closeWatch = treachery CloseWatch Cards.closeWatch

instance RunMessage CloseWatch where
  runMessage msg t@(CloseWatch attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> CloseWatch <$> runMessage msg attrs
