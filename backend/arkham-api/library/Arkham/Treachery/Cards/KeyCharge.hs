module Arkham.Treachery.Cards.KeyCharge (keyCharge) where

import Arkham.Campaigns.TheScarletKeys.Helpers (shift)
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Coterie))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype KeyCharge = KeyCharge TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keyCharge :: TreacheryCard KeyCharge
keyCharge = treachery KeyCharge Cards.keyCharge

instance RunMessage KeyCharge where
  runMessage msg t@(KeyCharge attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      skeys <- select $ ScarletKeyWithEnemyBearer $ EnemyWithTrait Coterie
      chooseOneAtATimeM iid $ targets skeys shift
      pure t
    _ -> KeyCharge <$> liftRunMessage msg attrs
