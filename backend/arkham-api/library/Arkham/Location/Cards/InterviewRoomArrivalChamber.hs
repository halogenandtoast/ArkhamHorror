module Arkham.Location.Cards.InterviewRoomArrivalChamber (interviewRoomArrivalChamber) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Yithian))

newtype InterviewRoomArrivalChamber = InterviewRoomArrivalChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

interviewRoomArrivalChamber :: LocationCard InterviewRoomArrivalChamber
interviewRoomArrivalChamber =
  location InterviewRoomArrivalChamber Cards.interviewRoomArrivalChamber 2 (Static 0)

instance HasAbilities InterviewRoomArrivalChamber where
  getAbilities (InterviewRoomArrivalChamber a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> exists (enemyAt a <> EnemyWithTrait Yithian))
      $ freeReaction (TurnBegins #after You)

instance RunMessage InterviewRoomArrivalChamber where
  runMessage msg l@(InterviewRoomArrivalChamber attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 1
      pure l
    _ -> InterviewRoomArrivalChamber <$> liftRunMessage msg attrs
