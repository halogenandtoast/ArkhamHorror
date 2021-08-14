module Arkham.Types.Enemy.Cards.ServantOfTheLurker
  ( servantOfTheLurker
  , ServantOfTheLurker(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Message
import Arkham.Types.Prey
import Arkham.Types.SkillType

newtype ServantOfTheLurker = ServantOfTheLurker EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

servantOfTheLurker :: EnemyCard ServantOfTheLurker
servantOfTheLurker = enemyWith
  ServantOfTheLurker
  Cards.servantOfTheLurker
  (4, Static 5, 2)
  (2, 2)
  (preyL .~ LowestSkill SkillAgility)

instance HasModifiersFor env ServantOfTheLurker

instance ActionRunner env => HasAbilities env ServantOfTheLurker where
  getAbilities i window (ServantOfTheLurker attrs) = getAbilities i window attrs

instance (EnemyRunner env) => RunMessage env ServantOfTheLurker where
  runMessage msg (ServantOfTheLurker attrs@EnemyAttrs {..}) = case msg of
    PerformEnemyAttack iid eid _ | eid == enemyId -> do
      push $ DiscardTopOfDeck iid 2 Nothing
      ServantOfTheLurker <$> runMessage msg attrs
    _ -> ServantOfTheLurker <$> runMessage msg attrs
