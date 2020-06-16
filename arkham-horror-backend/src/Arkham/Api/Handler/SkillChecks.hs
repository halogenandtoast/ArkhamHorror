module Arkham.Api.Handler.SkillChecks where

import Arkham.Fixtures
import Arkham.Types
import Import

getApiV1ArkhamGameSkillCheckR :: Int -> Handler ArkhamChaosToken
getApiV1ArkhamGameSkillCheckR gameId =
  liftIO $ loadGameFixture gameId >>= makeSkillCheck

makeSkillCheck :: ArkhamGame -> IO ArkhamChaosToken
makeSkillCheck = liftIO . drawFromChaosBag
