module Arkham.Treachery.Cards.TheScarletKeys.DealingsInTheDark.Shadowed (shadowed) where

import Arkham.Treachery.CardDefs.TheScarletKeys.DealingsInTheDark qualified as Cards
import Arkham.Treachery.Cards.TheForgottenAge.PnakoticBrotherhood.Shadowed qualified as Base
import Arkham.Treachery.Import.Lifted

newtype Shadowed = Shadowed Base.Shadowed
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor, HasAbilities)

shadowed :: TreacheryCard Shadowed
shadowed = treachery (Shadowed . Base.Shadowed) Cards.shadowed

instance RunMessage Shadowed where
  runMessage msg (Shadowed inner) =
    Shadowed <$> runMessage msg inner
