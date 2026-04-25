module Arkham.Enemy.Cards.ElokossFaintEmbers (elokossFaintEmbers) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyClues))
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Projection

newtype ElokossFaintEmbers = ElokossFaintEmbers EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elokossFaintEmbers :: EnemyCard ElokossFaintEmbers
elokossFaintEmbers = enemy ElokossFaintEmbers Cards.elokossFaintEmbers (5, Static 5, 5) (3, 3)

instance HasModifiersFor ElokossFaintEmbers where
  getModifiersFor (ElokossFaintEmbers a) = do
    healthModifier <- perPlayer 5
    modifySelf a [HealthModifier healthModifier, CannotMove, CannotMakeAttacksOfOpportunity]

instance HasAbilities ElokossFaintEmbers where
  getAbilities (ElokossFaintEmbers a) =
    extend1 a $ fastAbility a 1 (ClueCost (PerPlayer 1)) OnSameLocation

instance RunMessage ElokossFaintEmbers where
  runMessage msg e@(ElokossFaintEmbers attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      clues <- field EnemyClues attrs.id
      placeClues (attrs.ability 1) attrs 1
      when (clues == 4) $ push R2
      pure e
    _ -> ElokossFaintEmbers <$> liftRunMessage msg attrs
