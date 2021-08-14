module Arkham.Types.Enemy.Cards.ConglomerationOfSpheres
  ( conglomerationOfSpheres
  , ConglomerationOfSpheres(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Game.Helpers
import Arkham.Types.Message
import Arkham.Types.Prey
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype ConglomerationOfSpheres = ConglomerationOfSpheres EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

conglomerationOfSpheres :: EnemyCard ConglomerationOfSpheres
conglomerationOfSpheres = enemyWith
  ConglomerationOfSpheres
  Cards.conglomerationOfSpheres
  (1, Static 6, 4)
  (1, 1)
  (preyL .~ LowestSkill SkillWillpower)

instance HasModifiersFor env ConglomerationOfSpheres

instance ActionRunner env => HasAbilities env ConglomerationOfSpheres where
  getAbilities i window (ConglomerationOfSpheres attrs) =
    getAbilities i window attrs

instance EnemyRunner env => RunMessage env ConglomerationOfSpheres where
  runMessage msg e@(ConglomerationOfSpheres attrs@EnemyAttrs {..}) =
    case msg of
      After (FightEnemy _ eid source _ _) | eid == enemyId -> do
        traits <- getSet source
        e <$ when
          (Melee `member` traits)
          (push $ Discard $ sourceToTarget source)
      _ -> ConglomerationOfSpheres <$> runMessage msg attrs
