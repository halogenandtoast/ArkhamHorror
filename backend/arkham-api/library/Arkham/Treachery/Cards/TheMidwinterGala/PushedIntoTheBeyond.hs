module Arkham.Treachery.Cards.TheMidwinterGala.PushedIntoTheBeyond (pushedIntoTheBeyond) where

import Arkham.Treachery.CardDefs.TheMidwinterGala qualified as Cards
import Arkham.Treachery.Cards.TheDunwichLegacy.TheBeyond.PushedIntoTheBeyond qualified as Base
import Arkham.Treachery.Import.Lifted

newtype PushedIntoTheBeyond
  = PushedIntoTheBeyond Base.PushedIntoTheBeyond
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pushedIntoTheBeyond :: TreacheryCard PushedIntoTheBeyond
pushedIntoTheBeyond =
  treachery
    (PushedIntoTheBeyond . Base.PushedIntoTheBeyond)
    Cards.pushedIntoTheBeyond

instance RunMessage PushedIntoTheBeyond where
  runMessage msg (PushedIntoTheBeyond inner) =
    PushedIntoTheBeyond <$> runMessage msg inner
