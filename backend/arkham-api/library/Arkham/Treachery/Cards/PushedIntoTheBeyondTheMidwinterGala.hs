module Arkham.Treachery.Cards.PushedIntoTheBeyondTheMidwinterGala (pushedIntoTheBeyondTheMidwinterGala) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Cards.PushedIntoTheBeyond
import Arkham.Treachery.Import.Lifted

newtype PushedIntoTheBeyondTheMidwinterGala = PushedIntoTheBeyondTheMidwinterGala PushedIntoTheBeyond
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pushedIntoTheBeyondTheMidwinterGala :: TreacheryCard PushedIntoTheBeyondTheMidwinterGala
pushedIntoTheBeyondTheMidwinterGala = treachery (PushedIntoTheBeyondTheMidwinterGala . PushedIntoTheBeyond) Cards.pushedIntoTheBeyondTheMidwinterGala

instance RunMessage PushedIntoTheBeyondTheMidwinterGala where
  runMessage msg (PushedIntoTheBeyondTheMidwinterGala inner) =
    PushedIntoTheBeyondTheMidwinterGala <$> runMessage msg inner
