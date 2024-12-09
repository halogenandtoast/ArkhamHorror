module Arkham.Location.Cards.SawboneAlley (sawboneAlley, SawboneAlley (..)) where

import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Humanoid))

newtype SawboneAlley = SawboneAlley LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

sawboneAlley :: LocationCard SawboneAlley
sawboneAlley = location SawboneAlley Cards.sawboneAlley 2 (PerPlayer 2)

instance HasModifiersFor SawboneAlley where
  getModifiersFor (SawboneAlley a) =
    modifySelect a (enemyAt a <> EnemyWithTrait Humanoid) [EnemyFight 2, EnemyEvade (-2)]

instance RunMessage SawboneAlley where
  runMessage msg (SawboneAlley attrs) = runQueueT $ case msg of
    _ -> SawboneAlley <$> liftRunMessage msg attrs
