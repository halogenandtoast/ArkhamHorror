module Arkham.Types.Enemy.Cards.GhoulPriest where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.GameValue
import Arkham.Types.Prey
import Arkham.Types.SkillType

newtype GhoulPriest = GhoulPriest EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ghoulPriest :: EnemyCard GhoulPriest
ghoulPriest = enemy GhoulPriest Cards.ghoulPriest
  $ (healthDamageL .~ 2)
  . (sanityDamageL .~ 2)
  . (fightL .~ 4)
  . (healthL .~ PerPlayer 5)
  . (evadeL .~ 4)
  . (preyL .~ HighestSkill SkillCombat)

instance HasModifiersFor env GhoulPriest where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env GhoulPriest where
  getActions i window (GhoulPriest attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env GhoulPriest where
  runMessage msg (GhoulPriest attrs) = GhoulPriest <$> runMessage msg attrs
