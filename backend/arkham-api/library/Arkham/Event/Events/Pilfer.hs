module Arkham.Event.Events.Pilfer (pilfer) where

import Arkham.Aspect.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Investigate
import Arkham.Modifier

newtype Pilfer = Pilfer EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pilfer :: EventCard Pilfer
pilfer = event Pilfer Cards.pilfer

instance RunMessage Pilfer where
  runMessage msg e@(Pilfer attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (DiscoveredClues 2)
      aspect iid attrs (#agility `InsteadOf` #intellect) (mkInvestigate sid iid attrs)
      pure e
    _ -> Pilfer <$> liftRunMessage msg attrs
