module Arkham.Enemy.Cards.Medusa (medusa) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers (increaseThisFloodLevelOrElse)
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher

newtype Medusa = Medusa EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

medusa :: EnemyCard Medusa
medusa = enemy Medusa Cards.medusa

instance HasAbilities Medusa where
  getAbilities (Medusa a) =
    extend1 a $ restricted a 1 (thisIs a ReadyEnemy) $ forced $ PhaseBegins #when #enemy

instance RunMessage Medusa where
  runMessage msg e@(Medusa attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withLocationOf attrs \lid ->
        increaseThisFloodLevelOrElse lid $ placeDoom (attrs.ability 1) attrs 1
      pure e
    _ -> Medusa <$> liftRunMessage msg attrs
