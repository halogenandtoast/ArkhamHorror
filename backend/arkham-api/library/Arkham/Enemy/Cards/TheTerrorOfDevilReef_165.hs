module Arkham.Enemy.Cards.TheTerrorOfDevilReef_165 (
  theTerrorOfDevilReef_165,
  TheTerrorOfDevilReef_165 (..),
)
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modified)
import Arkham.Matcher
import Arkham.Trait (Trait (Cave))

newtype TheTerrorOfDevilReef_165 = TheTerrorOfDevilReef_165 EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theTerrorOfDevilReef_165 :: EnemyCard TheTerrorOfDevilReef_165
theTerrorOfDevilReef_165 = enemy TheTerrorOfDevilReef_165 Cards.theTerrorOfDevilReef_165 (3, Static 6, 3) (2, 2)

instance HasModifiersFor TheTerrorOfDevilReef_165 where
  getModifiersFor (LocationTarget lid) (TheTerrorOfDevilReef_165 a) = do
    isCave <- lid <=~> LocationWithTrait Cave
    modified a [CannotBeEnteredBy (be a) | isCave]
  getModifiersFor _ _ = pure []

instance RunMessage TheTerrorOfDevilReef_165 where
  runMessage msg (TheTerrorOfDevilReef_165 attrs) = runQueueT $ case msg of
    _ -> TheTerrorOfDevilReef_165 <$> liftRunMessage msg attrs
