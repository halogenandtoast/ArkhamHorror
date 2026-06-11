module Arkham.Treachery.Cards.OpenPortal (openPortal) where

import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype OpenPortal = OpenPortal TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

openPortal :: TreacheryCard OpenPortal
openPortal = treachery OpenPortal Cards.openPortal

instance RunMessage OpenPortal where
  runMessage msg t@(OpenPortal attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      assignDamage iid attrs 1
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 1
      tindalos <- selectOne $ locationIs Locations.tindalos
      for_ tindalos $ moveTo attrs iid
      pure t
    _ -> OpenPortal <$> liftRunMessage msg attrs
