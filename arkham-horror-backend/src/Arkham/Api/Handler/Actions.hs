
module Arkham.Api.Handler.Actions where

import Arkham.Internal.Card
import Arkham.Types
import Arkham.Util
import qualified Data.HashMap.Strict as HashMap
import Import
import Lens.Micro

-- brittany-disable-next-binding
applyAction :: ArkhamAction -> ArkhamGameData -> IO ArkhamGameData
applyAction action@(InvestigateAction investigation) g =
  -- TODO: Look at timing for when the action is spent, may need to finish action first
  pure $ g & gameStateStep .~ newGameStateStep & player . actions -~ 1
 where
  newGameStateStep = ArkhamGameStateStepSkillCheckStep $ ArkhamSkillCheckStep
    { ascsType = ArkhamSkillIntellect
    , ascsAction = Just action
    , ascsTarget = LocationTarget <$> mlocation
    }
  mlocation = lookup targetLocationId $ g ^. locations
  targetLocationId = aiaLocationId investigation
applyAction (TakeResourceAction _) g = pure $ g & player . resources +~ 1 & player . actions -~ 1
applyAction (DrawCardAction _) g = pure $ drawCard g & player . actions -~ 1
applyAction (PlayCardAction (ArkhamPlayCardAction n)) g = do
  let mcard = g ^? player . hand . ix n
  case mcard of
    Nothing -> throwString "No card at that index"
    Just card -> do
      let
        Just ci = HashMap.lookup (card ^. cardCode) cardsInternal
        card' = aciPlay ci (g ^. gameState) card
        stateTransform = aciAfterPlay ci
        cardCost = fromMaybe 0 (aciCost ci)
        actionCost = aciActionCost ci (g ^. gameState)
        resolveCard = case aciType ci of
                        ArkhamCardTypeEvent -> player . discard %~ (card :)
                        _ -> player . inPlay %~ (++ [card'])
      pure $ g & resolveCard & player . hand %~ without n & player . resources -~ cardCost & player . actions -~ actionCost & gameState %~ stateTransform
applyAction _ g = pure g

without :: Int -> [a] -> [a]
without n as = [ a | (i, a) <- zip [0 ..] as, i /= n ]

postApiV1ArkhamGameActionR :: ArkhamGameId -> Handler ArkhamGameData
postApiV1ArkhamGameActionR gameId = do
  game <- runDB $ get404 gameId
  action <- requireCheckJsonBody
  newGame <- liftIO $ traverseOf currentData (applyAction action) game
  runDB $ replace gameId newGame
  pure $ arkhamGameCurrentData newGame
