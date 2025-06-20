module Arkham.Treachery.Cards.SecretDoorTheMidwinterGala (secretDoorTheMidwinterGala) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Cards.SecretDoor
import Arkham.Treachery.Import.Lifted

newtype SecretDoorTheMidwinterGala = SecretDoorTheMidwinterGala SecretDoor
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor, HasAbilities)

secretDoorTheMidwinterGala :: TreacheryCard SecretDoorTheMidwinterGala
secretDoorTheMidwinterGala = treachery (SecretDoorTheMidwinterGala . SecretDoor) Cards.secretDoorTheMidwinterGala

instance RunMessage SecretDoorTheMidwinterGala where
  runMessage msg (SecretDoorTheMidwinterGala inner) = SecretDoorTheMidwinterGala <$> runMessage msg inner
