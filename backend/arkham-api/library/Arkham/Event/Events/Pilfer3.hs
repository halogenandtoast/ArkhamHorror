module Arkham.Event.Events.Pilfer3 (pilfer3) where

import Arkham.Aspect (InsteadOf(..))
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Investigate
import Arkham.Modifier

newtype Pilfer3 = Pilfer3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pilfer3 :: EventCard Pilfer3
pilfer3 = event Pilfer3 Cards.pilfer3

instance RunMessage Pilfer3 where
  runMessage msg e@(Pilfer3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (DiscoveredClues 2)
      aspect iid attrs (#agility `InsteadOf` #intellect) (mkInvestigate sid iid attrs)
      pure e
    PassedThisSkillTestBy iid (isSource attrs -> True) n | n >= 2 -> do
      atEndOfTurn attrs iid $ addToHand iid (only $ toCard attrs)
      pure e
    _ -> Pilfer3 <$> liftRunMessage msg attrs
