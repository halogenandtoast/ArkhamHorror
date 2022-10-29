module Arkham.Location.Cards.InterviewRoomArrivalChamber
  ( interviewRoomArrivalChamber
  , InterviewRoomArrivalChamber(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Trait ( Trait (Yithian) )

newtype InterviewRoomArrivalChamber = InterviewRoomArrivalChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

interviewRoomArrivalChamber :: LocationCard InterviewRoomArrivalChamber
interviewRoomArrivalChamber = location
  InterviewRoomArrivalChamber
  Cards.interviewRoomArrivalChamber
  2
  (Static 0)

instance HasAbilities InterviewRoomArrivalChamber where
  getAbilities (InterviewRoomArrivalChamber attrs) = withBaseAbilities
    attrs
    [ restrictedAbility
          attrs
          1
          (Here <> EnemyCriteria
            (EnemyExists $ enemyAt (toId attrs) <> EnemyWithTrait Yithian)
          )
        $ ReactionAbility (TurnBegins Timing.After You) Free
    ]

instance RunMessage InterviewRoomArrivalChamber where
  runMessage msg l@(InterviewRoomArrivalChamber attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ DrawCards iid 1 False
      pure l
    _ -> InterviewRoomArrivalChamber <$> runMessage msg attrs
