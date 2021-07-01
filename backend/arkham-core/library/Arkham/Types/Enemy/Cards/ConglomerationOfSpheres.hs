module Arkham.Types.Enemy.Cards.ConglomerationOfSpheres where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Prey
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype ConglomerationOfSpheres = ConglomerationOfSpheres EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

conglomerationOfSpheres :: EnemyCard ConglomerationOfSpheres
conglomerationOfSpheres = enemy ConglomerationOfSpheres Cards.conglomerationOfSpheres
  $ (healthDamageL .~ 1)
  . (sanityDamageL .~ 1)
  . (fightL .~ 1)
  . (healthL .~ Static 6)
  . (evadeL .~ 4)
  . (preyL .~ LowestSkill SkillWillpower)

instance HasModifiersFor env ConglomerationOfSpheres where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ConglomerationOfSpheres where
  getActions i window (ConglomerationOfSpheres attrs) =
    getActions i window attrs

instance EnemyRunner env => RunMessage env ConglomerationOfSpheres where
  runMessage msg e@(ConglomerationOfSpheres attrs@EnemyAttrs {..}) = case msg of
    After (FightEnemy _ eid source _ _) | eid == enemyId -> do
      traits <- getSet source
      e <$ when
        (Melee `member` traits)
        (unshiftMessage $ Discard $ sourceToTarget source)
    _ -> ConglomerationOfSpheres <$> runMessage msg attrs
