module Arkham.Types.Enemy.Cards.RelentlessDarkYoung where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Prey
import Arkham.Types.SkillType

newtype RelentlessDarkYoung = RelentlessDarkYoung EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

relentlessDarkYoung :: EnemyCard RelentlessDarkYoung
relentlessDarkYoung = enemy RelentlessDarkYoung Cards.relentlessDarkYoung
  $ (healthDamageL .~ 2)
  . (sanityDamageL .~ 1)
  . (fightL .~ 4)
  . (healthL .~ Static 5)
  . (evadeL .~ 2)
  . (preyL .~ LowestSkill SkillAgility)

instance HasModifiersFor env RelentlessDarkYoung where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env RelentlessDarkYoung where
  getActions i window (RelentlessDarkYoung attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env RelentlessDarkYoung where
  runMessage msg (RelentlessDarkYoung attrs) = case msg of
    EndRound ->
      pure $ RelentlessDarkYoung $ attrs & damageL %~ max 0 . subtract 2
    _ -> RelentlessDarkYoung <$> runMessage msg attrs
