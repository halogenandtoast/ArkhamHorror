module Arkham.Location.Cards.InterviewRoomRestrainingChamber (
  interviewRoomRestrainingChamber,
  InterviewRoomRestrainingChamber (..),
) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Prelude
import Arkham.ScenarioLogKey

newtype InterviewRoomRestrainingChamber = InterviewRoomRestrainingChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

interviewRoomRestrainingChamber :: LocationCard InterviewRoomRestrainingChamber
interviewRoomRestrainingChamber =
  location InterviewRoomRestrainingChamber Cards.interviewRoomRestrainingChamber 4 (PerPlayer 1)

instance HasAbilities InterviewRoomRestrainingChamber where
  getAbilities (InterviewRoomRestrainingChamber attrs) =
    withBaseAbilities attrs [restrictedAbility attrs 1 Here parleyAction_]

instance RunMessage InterviewRoomRestrainingChamber where
  runMessage msg l@(InterviewRoomRestrainingChamber attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      player <- getPlayer iid
      sid <- getRandom
      push
        $ chooseOne
          player
          [ SkillLabel #intellect [parley sid iid (attrs.ability 1) iid #intellect (Fixed 4)]
          , SkillLabel #combat [parley sid iid (attrs.ability 1) iid #combat (Fixed 4)]
          ]
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      push $ Remember InterviewedASubject
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ assignHorror iid attrs 2
      pure l
    _ -> InterviewRoomRestrainingChamber <$> runMessage msg attrs
