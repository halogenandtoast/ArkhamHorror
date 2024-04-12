module Arkham.Location.Cards.InterviewRoomIchorFilledChamber (
  interviewRoomIchorFilledChamber,
  InterviewRoomIchorFilledChamber (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype InterviewRoomIchorFilledChamber = InterviewRoomIchorFilledChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

interviewRoomIchorFilledChamber :: LocationCard InterviewRoomIchorFilledChamber
interviewRoomIchorFilledChamber =
  location
    InterviewRoomIchorFilledChamber
    Cards.interviewRoomIchorFilledChamber
    1
    (PerPlayer 1)

instance HasAbilities InterviewRoomIchorFilledChamber where
  getAbilities (InterviewRoomIchorFilledChamber a) =
    withBaseAbilities a
      $ [ mkAbility a 1
            $ ForcedAbility
            $ Enters Timing.After You
            $ LocationWithId
            $ toId a
        ]

instance RunMessage InterviewRoomIchorFilledChamber where
  runMessage msg l@(InterviewRoomIchorFilledChamber attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push
        $ beginSkillTest
          iid
          (attrs.ability 1)
          (InvestigatorTarget iid)
          SkillWillpower
          (Fixed 3)
      pure l
    FailedSkillTest iid _ (isAbilitySource attrs 1 -> True) SkillTestInitiatorTarget {} _ n ->
      do
        push $ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 n
        pure l
    _ -> InterviewRoomIchorFilledChamber <$> runMessage msg attrs
