module Arkham.Location.Cards.InterviewRoomArrivalChamber (
  interviewRoomArrivalChamber,
  InterviewRoomArrivalChamber (..),
) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait (Trait (Yithian))

newtype InterviewRoomArrivalChamber = InterviewRoomArrivalChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

interviewRoomArrivalChamber :: LocationCard InterviewRoomArrivalChamber
interviewRoomArrivalChamber =
  location InterviewRoomArrivalChamber Cards.interviewRoomArrivalChamber 2 (Static 0)

instance HasAbilities InterviewRoomArrivalChamber where
  getAbilities (InterviewRoomArrivalChamber attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility attrs 1 (Here <> exists (enemyAt (toId attrs) <> EnemyWithTrait Yithian))
          $ freeReaction (TurnBegins #after You)
      ]

instance RunMessage InterviewRoomArrivalChamber where
  runMessage msg l@(InterviewRoomArrivalChamber attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ drawCards iid (attrs.ability 1) 1
      pure l
    _ -> InterviewRoomArrivalChamber <$> runMessage msg attrs
