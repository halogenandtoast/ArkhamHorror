module Arkham.Treachery.Cards.HuntDown (huntDown) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.WarOfTheOuterGods.Helpers
import Arkham.Trait (Trait (Mutated))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HuntDown = HuntDown TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntDown :: TreacheryCard HuntDown
huntDown = treachery HuntDown Cards.huntDown

instance RunMessage HuntDown where
  runMessage msg t@(HuntDown attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <- selectMaybeMax EnemyFight $ EnemyWithTrait Mutated
      if null enemies
        then gainSurge attrs
        else chooseOrRunOneM iid $ targets enemies $ handleTarget iid attrs
      pure t
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget enemy) -> do
      placeMutations attrs enemy 1
      readyThis enemy
      withLocationOf iid \lid -> do
        push $ MoveUntil lid (toTarget enemy)
        push $ EnemyEngageInvestigator enemy iid
        initiateEnemyAttack enemy attrs iid
      pure t
    _ -> HuntDown <$> liftRunMessage msg attrs
