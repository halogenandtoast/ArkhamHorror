module Arkham.Act.Cards.MeetAndGreet (meetAndGreet) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.SlotType
import Arkham.Trait

newtype MeetAndGreet = MeetAndGreet ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

meetAndGreet :: ActCard MeetAndGreet
meetAndGreet = act (1, A) MeetAndGreet Cards.meetAndGreet $ groupClueCost (PerPlayer 3)

instance HasModifiersFor MeetAndGreet where
  getModifiersFor (MeetAndGreet a) =
    modifySelect a (AssetWithTrait Guest) [DoNotTakeUpSlot AllySlot]

instance RunMessage MeetAndGreet where
  runMessage msg (MeetAndGreet attrs) = runQueueT $ case msg of
    -- TODO implement parley abilities and advancement
    _ -> MeetAndGreet <$> liftRunMessage msg attrs
