{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Campaigns.TheScarletKeys.Key.Runner (module X) where

import Arkham.Calculation as X
import Arkham.Campaigns.TheScarletKeys.Key.Types as X
import Arkham.Classes as X
import Arkham.Helpers.Effect as X
import Arkham.Helpers.Message as X hiding (story)
import Arkham.Helpers.Query as X
import Arkham.Id as X
import Arkham.SkillTest.Base as X (SkillTestDifficulty (..))
import Arkham.Source as X
import Arkham.Target as X

import Arkham.Prelude hiding (Key)

instance RunMessage Key where
  runMessage msg (Key a) = Key <$> runMessage msg a

instance RunMessage KeyAttrs where
  runMessage _msg attrs = pure attrs
