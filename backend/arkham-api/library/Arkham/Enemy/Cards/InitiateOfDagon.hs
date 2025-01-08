module Arkham.Enemy.Cards.InitiateOfDagon (initiateOfDagon, InitiateOfDagon (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyDoom))
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Matcher
import Arkham.Projection

newtype InitiateOfDagon = InitiateOfDagon EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

initiateOfDagon :: EnemyCard InitiateOfDagon
initiateOfDagon = enemyWith InitiateOfDagon Cards.initiateOfDagon (2, Static 1, 2) (0, 1) spawnAtEmptyLocation

instance HasModifiersFor InitiateOfDagon where
  getModifiersFor (InitiateOfDagon a) = do
    noDoom <- fieldMap EnemyDoom (== 0) a.id
    modifySelfWhen a noDoom [EnemyFight 2, EnemyEvade 2]

instance HasAbilities InitiateOfDagon where
  getAbilities (InitiateOfDagon a) =
    extend1 a $ restrictedAbility a 1 (thisIs a $ EnemyWithoutDoom <> #ready) (forced $ RoundEnds #when)

instance RunMessage InitiateOfDagon where
  runMessage msg e@(InitiateOfDagon attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure e
    _ -> InitiateOfDagon <$> liftRunMessage msg attrs
