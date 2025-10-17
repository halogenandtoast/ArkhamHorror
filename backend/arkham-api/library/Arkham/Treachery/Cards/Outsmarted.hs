module Arkham.Treachery.Cards.Outsmarted (outsmarted) where

import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.SanguineShadows.Helpers
import Arkham.Trait (Trait (Coterie))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Outsmarted = Outsmarted TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outsmarted :: TreacheryCard Outsmarted
outsmarted = treachery Outsmarted Cards.outsmarted

instance RunMessage Outsmarted where
  runMessage msg t@(Outsmarted attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #intellect (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      coterie <- select $ NearestEnemyToFallback iid $ EnemyWithTrait Coterie
      chooseOneM iid do
        withI18n $ countVar 1 $ labeled' "placeAgendaDoomCanAdvance" $ placeDoomOnAgendaAndCheckAdvance 1
        scenarioI18n $ labeledValidate' (notNull coterie) "coterieAttack" do
          chooseTargetM iid coterie \enemy -> initiateEnemyAttack enemy attrs iid
      pure t
    _ -> Outsmarted <$> liftRunMessage msg attrs
