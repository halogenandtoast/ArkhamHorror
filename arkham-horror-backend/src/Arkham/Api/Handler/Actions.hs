module Arkham.Api.Handler.Actions
  ( postApiV1ArkhamGameActionR
  )
where

import Arkham.Internal.Location
import Arkham.Internal.PlayerCard
import Arkham.Internal.Scenario
import Arkham.Types
import Arkham.Types.Action
import Arkham.Types.Game
import Arkham.Types.GameState
import Arkham.Types.Location
import Arkham.Types.Player
import Arkham.Types.Skill
import Arkham.Util
import qualified Data.HashSet as HashSet
import Import
import Lens.Micro
import Safe (fromJustNote)

-- brittany-disable-next-binding
applyAction :: ArkhamAction -> ArkhamGameData -> IO ArkhamGameData
applyAction action@(EvadeEnemyAction _) g =
  pure $ g & gameStateStep .~ newGameStateStep & activePlayer . actions -~ 1
    where
      newGameStateStep = ArkhamGameStateStepSkillCheckStep $ ArkhamSkillCheckStep
        { ascsType = ArkhamSkillAgility
        , ascsAction = action
        }
applyAction action@(FightEnemyAction _) g =
  pure $ g & gameStateStep .~ newGameStateStep & activePlayer . actions -~ 1
    where
      newGameStateStep = ArkhamGameStateStepSkillCheckStep $ ArkhamSkillCheckStep
        { ascsType = ArkhamSkillCombat
        , ascsAction = action
        }
applyAction (MoveAction move) g = do
  let cardCode' = amaTo move
      currentPlayer = g ^. activePlayer
      currentLocation = locationFor currentPlayer (g ^. gameState)
      currentLocationId = alCardCode currentLocation
      currentLocation' = currentLocation & investigators %~ HashSet.delete (_playerId currentPlayer)
      newLocation = fromJustNote "No known location" (g ^? locations . ix cardCode')
      newLocationInternal = lookupLocationInternal cardCode'
      revealLocation = if alStatus newLocation == Unrevealed then aliOnReveal newLocationInternal (g ^. gameState) else id
      location' = revealLocation newLocation & investigators .~ HashSet.singleton (_playerId currentPlayer)
      g' = g & locations . at cardCode' ?~ location' & locations . at currentLocationId ?~ currentLocation'
  (s, p) <- aliOnEnter newLocationInternal (g' ^. gameState) currentPlayer
  pure $ g' & gameState .~ s & activePlayer .~ (p & actions -~ 1)
applyAction action@(InvestigateAction _) g =
  -- TODO: Look at timing for when the action is spent, may need to finish action first
  pure $ g & gameStateStep .~ newGameStateStep & activePlayer . actions -~ 1
 where
  newGameStateStep = ArkhamGameStateStepSkillCheckStep $ ArkhamSkillCheckStep
    { ascsType = ArkhamSkillIntellect
    , ascsAction = action
    }
applyAction (TakeResourceAction _) g = pure $ g & activePlayer . resources +~ 1 & activePlayer . actions -~ 1
applyAction (DrawCardAction _) g = pure $ drawCard g & activePlayer . actions -~ 1
applyAction (PlayCardAction (ArkhamPlayCardAction n)) g = do
  let mcard = g ^? activePlayer . hand . ix n
  case mcard of
    Nothing -> throwString "No card at that index"
    Just card -> do
      let
        Just ci = toInternalPlayerCard card
        card' = aciPlay ci (g ^. gameState) card
        stateTransform = aciAfterPlay ci
        cardCost = fromMaybe 0 (aciCost ci)
        actionCost = aciActionCost ci (g ^. gameState)
        resolveCard = case aciType ci of
                        PlayerEvent -> activePlayer . discard %~ (card :)
                        _ -> activePlayer . inPlay %~ (++ [card'])
      pure $ g
        & resolveCard
        & activePlayer . hand %~ without n
        & activePlayer . resources -~ cardCost
        & activePlayer . actions -~ actionCost
        & gameState %~ stateTransform
applyAction _ g = pure g

without :: Int -> [a] -> [a]
without n as = [ a | (i, a) <- zip [0 ..] as, i /= n ]

postApiV1ArkhamGameActionR :: ArkhamGameId -> Handler ArkhamGameData
postApiV1ArkhamGameActionR gameId = do
  game <- runDB $ get404 gameId
  action <- requireCheckJsonBody
  newGame <- liftIO $ traverseOf currentData (applyAction action) game
  runDB $ updateGame gameId newGame
