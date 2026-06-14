module Arkham.Location.Cards.DrownedShanty (drownedShanty) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen, modifySelfWhen)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (DeepOne))

newtype DrownedShanty = DrownedShanty LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drownedShanty :: LocationCard DrownedShanty
drownedShanty = location DrownedShanty Cards.drownedShanty 0 (Static 2)

instance HasModifiersFor DrownedShanty where
  getModifiersFor (DrownedShanty a) = whenRevealed a do
    deepOneMoving <- selectAny (MovingEnemy <> EnemyWithTrait DeepOne)
    -- While a Deep One enemy is moving, Drowned Shanty is connected to every
    -- fully flooded location and vice versa.
    modifySelfWhen a deepOneMoving [ConnectedToWhen (be a) FullyFloodedLocation]
    modifySelectWhen a deepOneMoving FullyFloodedLocation [ConnectedToWhen FullyFloodedLocation (be a)]

instance RunMessage DrownedShanty where
  runMessage msg (DrownedShanty attrs) = runQueueT $ DrownedShanty <$> liftRunMessage msg attrs
