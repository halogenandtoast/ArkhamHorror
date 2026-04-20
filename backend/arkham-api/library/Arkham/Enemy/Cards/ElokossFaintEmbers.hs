module Arkham.Enemy.Cards.ElokossFaintEmbers (elokossFaintEmbers) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.GameValue

newtype ElokossFaintEmbers = ElokossFaintEmbers EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

elokossFaintEmbers :: EnemyCard ElokossFaintEmbers
elokossFaintEmbers = enemy ElokossFaintEmbers Cards.elokossFaintEmbers (5, Static 5, 5) (3, 3)

instance HasModifiersFor ElokossFaintEmbers where
  getModifiersFor (ElokossFaintEmbers a) = do
    healthModifier <- perPlayer 5
    modifySelf a [HealthModifier healthModifier, CannotMove, CannotMakeAttacksOfOpportunity]


instance RunMessage ElokossFaintEmbers where
  runMessage msg (ElokossFaintEmbers attrs) = ElokossFaintEmbers <$> runMessage msg attrs
