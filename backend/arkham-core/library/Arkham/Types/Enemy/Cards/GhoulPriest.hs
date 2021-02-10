module Arkham.Types.Enemy.Cards.GhoulPriest where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.Prey
import Arkham.Types.SkillType
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype GhoulPriest = GhoulPriest EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ghoulPriest :: EnemyId -> GhoulPriest
ghoulPriest uuid =
  GhoulPriest
    $ baseAttrs uuid "01116"
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
