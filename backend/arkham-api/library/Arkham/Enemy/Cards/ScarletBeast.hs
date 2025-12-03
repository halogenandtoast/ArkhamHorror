module Arkham.Enemy.Cards.ScarletBeast (scarletBeast) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfMaybe)
import Arkham.Matcher.Location
import Arkham.Trait (Trait (LocusSite))

newtype ScarletBeast = ScarletBeast EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

scarletBeast :: EnemyCard ScarletBeast
scarletBeast = enemy ScarletBeast Cards.scarletBeast (2, Static 4, 4) (1, 1)

instance HasModifiersFor ScarletBeast where
  getModifiersFor (ScarletBeast a) = modifySelfMaybe a do
    loc <- MaybeT $ getLocationOf a
    liftGuardM $ matches loc $ LocationWithTrait LocusSite
    pure [EnemyFight 2, EnemyEvade (-2)]
