module Arkham.Treachery.Cards.TheMidwinterGala.SecretDoor (secretDoor) where

import Arkham.Treachery.CardDefs.TheMidwinterGala qualified as Cards
import Arkham.Treachery.Cards.ReturnToTheDunwichLegacy.SecretDoors.SecretDoor qualified as Base
import Arkham.Treachery.Import.Lifted

newtype SecretDoor = SecretDoor Base.SecretDoor
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor, HasAbilities)

secretDoor :: TreacheryCard SecretDoor
secretDoor =
  treachery (SecretDoor . Base.SecretDoor) Cards.secretDoor

instance RunMessage SecretDoor where
  runMessage msg (SecretDoor inner) = SecretDoor <$> runMessage msg inner
