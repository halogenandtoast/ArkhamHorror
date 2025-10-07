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

import Arkham.Prelude

instance RunMessage ScarletKey where
  runMessage msg (ScarletKey a) = ScarletKey <$> runMessage msg a

instance RunMessage ScarletKeyAttrs where
  runMessage msg attrs = case msg of
    PlaceScarletKey skid p | skid == attrs.id -> do
      pure $ attrs & placementL .~ p
    _ -> pure attrs
