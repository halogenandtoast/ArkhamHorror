module Arkham.Enemy.Cards.TheTerrorOfDevilReef_164 (
  theTerrorOfDevilReef_164,
  TheTerrorOfDevilReef_164 (..),
)
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modified)
import Arkham.Matcher
import Arkham.Trait (Trait (Cave))

newtype TheTerrorOfDevilReef_164 = TheTerrorOfDevilReef_164 EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theTerrorOfDevilReef_164 :: EnemyCard TheTerrorOfDevilReef_164
theTerrorOfDevilReef_164 = enemy TheTerrorOfDevilReef_164 Cards.theTerrorOfDevilReef_164 (3, Static 6, 3) (2, 2)

instance HasModifiersFor TheTerrorOfDevilReef_164 where
  getModifiersFor target (TheTerrorOfDevilReef_164 a) | isTarget a target = do
    n <- perPlayer 3
    modified a [HealthModifier n]
  getModifiersFor (LocationTarget lid) (TheTerrorOfDevilReef_164 a) = do
    isCave <- lid <=~> LocationWithTrait Cave
    modified a [CannotBeEnteredBy (be a) | isCave]
  getModifiersFor _ _ = pure []

instance RunMessage TheTerrorOfDevilReef_164 where
  runMessage msg (TheTerrorOfDevilReef_164 attrs) = runQueueT $ case msg of
    _ -> TheTerrorOfDevilReef_164 <$> liftRunMessage msg attrs
