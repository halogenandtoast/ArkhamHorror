module Arkham.Types.Enemy.Cards.ServantOfTheLurker where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Prey
import Arkham.Types.SkillType

newtype ServantOfTheLurker = ServantOfTheLurker EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

servantOfTheLurker :: EnemyCard ServantOfTheLurker
servantOfTheLurker = enemy ServantOfTheLurker Cards.servantOfTheLurker
  $ (healthDamageL .~ 2)
  . (sanityDamageL .~ 2)
  . (fightL .~ 4)
  . (healthL .~ Static 5)
  . (evadeL .~ 2)
  . (preyL .~ LowestSkill SkillAgility)

instance HasModifiersFor env ServantOfTheLurker where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ServantOfTheLurker where
  getActions i window (ServantOfTheLurker attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env ServantOfTheLurker where
  runMessage msg (ServantOfTheLurker attrs@EnemyAttrs {..}) = case msg of
    PerformEnemyAttack iid eid | eid == enemyId -> do
      unshiftMessage $ DiscardTopOfDeck iid 2 Nothing
      ServantOfTheLurker <$> runMessage msg attrs
    _ -> ServantOfTheLurker <$> runMessage msg attrs
