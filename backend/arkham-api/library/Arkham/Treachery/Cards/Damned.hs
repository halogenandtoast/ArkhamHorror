module Arkham.Treachery.Cards.Damned (damned) where

import Arkham.Classes
import Arkham.Prelude
import Arkham.Treachery.CardDefs.ReturnTo qualified as Cards
import Arkham.Treachery.Runner

newtype Damned = Damned TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

damned :: TreacheryCard Damned
damned = treachery Damned Cards.damned
