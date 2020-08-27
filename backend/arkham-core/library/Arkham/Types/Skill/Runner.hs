{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Skill.Runner where

import Arkham.Types.Classes

type SkillRunner env = (HasQueue env)
