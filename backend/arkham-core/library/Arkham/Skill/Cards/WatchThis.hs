module Arkham.Skill.Cards.WatchThis
  ( watchThis
  , WatchThis(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Skill.Cards as Cards
import Arkham.Classes
import Arkham.Skill.Attrs
import Arkham.Skill.Runner

newtype WatchThis = WatchThis SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

watchThis :: SkillCard WatchThis
watchThis =
  skill WatchThis Cards.watchThis

instance SkillRunner env => RunMessage env WatchThis where
  runMessage msg (WatchThis attrs) = WatchThis <$> runMessage msg attrs
