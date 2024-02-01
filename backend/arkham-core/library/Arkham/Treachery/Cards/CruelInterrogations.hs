module Arkham.Treachery.Cards.CruelInterrogations (
  cruelInterrogations,
  CruelInterrogations (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.ScenarioLogKey
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype CruelInterrogations = CruelInterrogations TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

cruelInterrogations :: TreacheryCard CruelInterrogations
cruelInterrogations = treachery CruelInterrogations Cards.cruelInterrogations

instance HasModifiersFor CruelInterrogations where
  getModifiersFor (InvestigatorTarget iid) (CruelInterrogations a)
    | treacheryOnInvestigator iid a = do
        pure $ toModifiers a [CannotTakeAction $ IsAction Action.Draw]
  getModifiersFor _ _ = pure []

instance HasAbilities CruelInterrogations where
  getAbilities (CruelInterrogations a) = [restrictedAbility a 1 OnSameLocation actionAbility]

instance RunMessage CruelInterrogations where
  runMessage msg t@(CruelInterrogations attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      interviewedASubject <- remembered InterviewedASubject
      pushAll
        $ AttachTreachery (toId t) (InvestigatorTarget iid)
        : ( guard interviewedASubject
              *> [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1
                 , gainSurge attrs
                 ]
          )
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ beginSkillTest iid (toAbilitySource attrs 1) iid #willpower 2
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) attrs
      pure t
    _ -> CruelInterrogations <$> runMessage msg attrs
