module Arkham.Treachery.Cards.ViolentCommandsTheMidwinterGala (violentCommandsTheMidwinterGala) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Cards.ViolentCommands
import Arkham.Treachery.Import.Lifted

newtype ViolentCommandsTheMidwinterGala = ViolentCommandsTheMidwinterGala ViolentCommands
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities, HasModifiersFor)

violentCommandsTheMidwinterGala :: TreacheryCard ViolentCommandsTheMidwinterGala
violentCommandsTheMidwinterGala = treachery (ViolentCommandsTheMidwinterGala . ViolentCommands) Cards.violentCommandsTheMidwinterGala

instance RunMessage ViolentCommandsTheMidwinterGala where
  runMessage msg (ViolentCommandsTheMidwinterGala inner) =
    ViolentCommandsTheMidwinterGala <$> runMessage msg inner
