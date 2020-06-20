module Arkham.Api.Handler.Actions where

import Arkham.Fixtures
import Arkham.Types
import Import
import Lens.Micro

  -- TODO: mark location, initiate intellect skill check against shroud
  -- skill check timing
  -- determine skill of test
  -- player window for fast actions
  -- Commit cards from hand
  -- player window
  -- Reveal token
  -- resolve token effects
  -- determine modified skill value
  -- determine success or failure
  -- apply results
  -- skill test ends
applyAction :: ArkhamGame -> ArkhamAction -> IO ArkhamGame
applyAction g action@(InvestigateAction investigation) =
  pure $ g & gameStateStep .~ newGameStateStep
 where
  newGameStateStep = ArkhamGameStateStepSkillCheckStep $ ArkhamSkillCheckStep
    { ascsType = ArkhamSkillIntellect
    , ascsAction = Just action
    , ascsTarget = LocationTarget . RevealedLocation <$> mlocation
    }
  mlocation = findLocation $ [ l | RevealedLocation l <- g ^. locations ]
  targetLocationId = aiaLocationId investigation
  findLocation = find ((== targetLocationId) . (^. locationId))
applyAction g _ = pure g

postApiV1ArkhamGameActionR :: Int -> Handler ArkhamGame
postApiV1ArkhamGameActionR gameId = do
  game <- liftIO $ loadGameFixture gameId
  action <- requireCheckJsonBody
  liftIO $ applyAction game action
