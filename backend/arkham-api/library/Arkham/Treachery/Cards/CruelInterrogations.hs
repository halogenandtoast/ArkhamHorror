module Arkham.Treachery.Cards.CruelInterrogations (cruelInterrogations, CruelInterrogations (..)) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.ScenarioLogKey
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CruelInterrogations = CruelInterrogations TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cruelInterrogations :: TreacheryCard CruelInterrogations
cruelInterrogations = treachery CruelInterrogations Cards.cruelInterrogations

instance HasModifiersFor CruelInterrogations where
  getModifiersFor (InvestigatorTarget iid) (CruelInterrogations a) | treacheryInThreatArea iid a = do
    modified a [CannotTakeAction $ IsAction Action.Draw]
  getModifiersFor _ _ = pure []

instance HasAbilities CruelInterrogations where
  getAbilities (CruelInterrogations a) = [skillTestAbility $ restrictedAbility a 1 OnSameLocation actionAbility]

instance RunMessage CruelInterrogations where
  runMessage msg t@(CruelInterrogations attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      interviewedASubject <- remembered InterviewedASubject
      placeInThreatArea attrs iid
      when interviewedASubject do
        assignHorror iid attrs 1
        gainSurge attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 2)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> CruelInterrogations <$> liftRunMessage msg attrs
