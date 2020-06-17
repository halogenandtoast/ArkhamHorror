module Arkham.Api.Handler.SkillChecks where

import Arkham.Fixtures
import Arkham.Types
import Import

getApiV1ArkhamGameSkillCheckR :: Int -> Handler ArkhamChaosToken
getApiV1ArkhamGameSkillCheckR gameId =
  liftIO $ loadGameFixture gameId >>= makeSkillCheck

makeSkillCheck :: ArkhamGame -> IO ArkhamChaosToken
makeSkillCheck = liftIO . drawFromChaosBag

data ArkhamSkillCheckResult = Success | Failure
  deriving stock (Generic)
  deriving anyclass (ToJSON)

revealedLocations :: ArkhamGame -> [ArkhamRevealedLocation]
revealedLocations game = do
  RevealedLocation l <- agsLocations $ agGameState game
  pure l

postApiV1ArkhamGameInvestigateR :: Int -> Text -> Handler ArkhamSkillCheckResult
postApiV1ArkhamGameInvestigateR gameId locationIdText = do
  game <- liftIO $ loadGameFixture gameId
  case findRequestLocation game of
    Just location -> pure Success
    Nothing -> pure Failure
  where
    locationId = LocationId locationIdText
    findRequestLocation = find ((== locationId) . getLocationId) . revealedLocations
