module Arkham.Api.Handler.SkillChecks
  ( postApiV1ArkhamGameSkillCheckR
  , postApiV1ArkhamGameSkillCheckApplyResultR
  )
where

import Arkham.Conversion
import Arkham.Internal.PlayerCard
import Arkham.Internal.Types
import Arkham.Internal.Util
import Arkham.Types
import Arkham.Types.Action
import Arkham.Types.Card
import Arkham.Types.ChaosToken
import Arkham.Types.Enemy
import Arkham.Types.Game
import Arkham.Types.GameState
import Arkham.Types.Investigator
import Arkham.Types.Location
import Arkham.Types.Player
import Arkham.Types.Skill
import Arkham.Util
import qualified Data.HashMap.Strict as HashMap
import Data.UUID
import GHC.Stack
import Import
import Lens.Micro
import Lens.Micro.Platform ()
import Safe (fromJustNote)

getLocation :: HasLocations a => a -> ArkhamCardCode -> ArkhamLocation
getLocation g locationId =
  fromJustNote "could not find location" $ g ^? locations . ix locationId

postApiV1ArkhamGameSkillCheckR :: ArkhamGameId -> Handler ArkhamGameData
postApiV1ArkhamGameSkillCheckR gameId = do
  game <- runDB $ get404 gameId
  cards <- requireCheckJsonBody
  let ArkhamGameStateStepSkillCheckStep step = game ^. gameStateStep

  case ascsAction step of
    (EvadeEnemyAction action) -> evadeAction
      (Entity gameId game)
      ArkhamSkillAgility
      (aeveaEnemyId action)
      cards
    (FightEnemyAction action) -> fightAction
      (Entity gameId game)
      ArkhamSkillCombat
      (afeaEnemyId action)
      cards
    (InvestigateAction action) -> do
      let
        Just location =
          HashMap.lookup (aiaLocationId action) (game ^. locations)
      investigateAction (Entity gameId game) ArkhamSkillIntellect location cards
    _ -> error "fail"

getDifficulty :: MonadIO m => ArkhamGame -> ArkhamAction -> m Int
getDifficulty g action = case action of
  (FightEnemyAction a) -> fightOf g (afeaEnemyId a)
  (EvadeEnemyAction a) -> evadeOf g (aeveaEnemyId a)
  (InvestigateAction a) -> shroudOf g $ getLocation g (aiaLocationId a)
  _ -> error "Can not get difficulty for action"


postApiV1ArkhamGameSkillCheckApplyResultR
  :: ArkhamGameId -> Handler ArkhamGameData
postApiV1ArkhamGameSkillCheckApplyResultR gameId = do
  game <- runDB $ get404 gameId
  let
    ArkhamGameStateStepRevealTokenStep ArkhamRevealTokenStep {..} =
      game ^. gameStateStep
    tokenInternal = toInternalToken game artsToken

  case
      tokenToResult
        tokenInternal
        (game ^. currentData . gameState)
        (game ^. activePlayer)
    of
      Modifier n -> do
        checkDifficulty <- getDifficulty game artsAction
        modifiedSkillValue <- determineModifiedSkillValue
          artsType
          (game ^. activePlayer)
          artsCards
          n
        if modifiedSkillValue >= checkDifficulty
          then runDB $ updateGame gameId =<< successfulCheck game artsAction
          else runDB $ updateGame gameId $ failedCheck game artsAction
      Failure -> runDB $ updateGame gameId $ failedCheck game artsAction

shroudOf :: MonadIO m => ArkhamGame -> ArkhamLocation -> m Int
shroudOf _ location = pure $ alShroud location

determineModifiedSkillValue
  :: MonadIO m
  => ArkhamSkillType
  -> ArkhamPlayer
  -> [ArkhamCard]
  -> Int
  -> m Int
determineModifiedSkillValue skillType player' commitedCards tokenModifier =
  pure $ skillValue player' skillType + cardContributions + tokenModifier
 where
  cardContributions = length $ filter (== skillType) $ concatMap
    (maybe [] aciTestIcons . toInternalPlayerCard)
    commitedCards

skillValue :: ArkhamPlayer -> ArkhamSkillType -> Int
skillValue p skillType = case skillType of
  ArkhamSkillWillpower -> unArkhamSkill $ aiWillpower $ _investigator p
  ArkhamSkillIntellect -> unArkhamSkill $ aiIntellect $ _investigator p
  ArkhamSkillCombat -> unArkhamSkill $ aiCombat $ _investigator p
  ArkhamSkillAgility -> unArkhamSkill $ aiAgility $ _investigator p
  ArkhamSkillWild -> error "Not a possible skill"

revealToken
  :: ArkhamChaosToken
  -> Int
  -> Int
  -> [ArkhamCard]
  -> ArkhamGameStateStep
  -> ArkhamGameStateStep
revealToken token' checkDifficulty modifiedSkillValue cards (ArkhamGameStateStepSkillCheckStep ArkhamSkillCheckStep {..})
  = ArkhamGameStateStepRevealTokenStep $ ArkhamRevealTokenStep
    ascsType
    ascsAction
    token'
    checkDifficulty
    modifiedSkillValue
    cards
revealToken _ _ _ _ s = s

investigateAction
  :: Entity ArkhamGame
  -> ArkhamSkillType
  -> ArkhamLocation
  -> [Int]
  -> Handler ArkhamGameData
investigateAction (Entity gameId game) skillType location cardIndexes = do
  token' <- liftIO $ drawChaosToken game
  checkDifficulty <- shroudOf game location
  let
    tokenModifier = tokenToModifier game (game ^. activePlayer) token'
    hand' = game ^. activePlayer . hand
    (commitedCards, remainingCards) =
      over both (map snd) $ partition (\(i, _) -> i `elem` cardIndexes) $ zip
        [0 ..]
        hand'
  modifiedSkillValue <- determineModifiedSkillValue
    skillType
    (game ^. activePlayer)
    commitedCards
    tokenModifier
  let
    newGame =
      game
        & gameStateStep
        %~ revealToken token' checkDifficulty modifiedSkillValue commitedCards
        & activePlayer
        . hand
        .~ remainingCards
        & activePlayer
        . discard
        %~ (commitedCards ++)
  runDB $ updateGame gameId newGame

failedCheck :: ArkhamGame -> ArkhamAction -> ArkhamGame
failedCheck g _ = g & gameStateStep .~ investigatorStep

successfulCheck
  :: (HasCallStack, MonadIO m) => ArkhamGame -> ArkhamAction -> m ArkhamGame
successfulCheck g action = case action of
  (InvestigateAction a) ->
    successfulInvestigation g (getLocation g (aiaLocationId a)) 1
  (FightEnemyAction a) -> successfulFight g (afeaEnemyId a) 1
  _ -> error "Unknown check"

-- brittany-disable-next-binding
successfulInvestigation :: MonadIO m => ArkhamGame -> ArkhamLocation -> Int -> m ArkhamGame
successfulInvestigation g l clueCount = pure $ g
  & locations . at (alCardCode l) . _Just . clues -~ clueCount
  & activePlayer . clues +~ clueCount
  & gameStateStep .~ investigatorStep

successfulFight :: MonadIO m => ArkhamGame -> UUID -> Int -> m ArkhamGame
successfulFight g enemyId damage' =
  if _enemyDamage enemy'' >= _enemyHealth enemy''
    then discardEnemy g enemyId <&> gameStateStep .~ investigatorStep
    else
      pure
      $ g
      & enemies
      . at enemyId
      ?~ enemy''
      & gameStateStep
      .~ investigatorStep
 where
  enemy' = fromJustNote "could not find enemy" $ g ^? enemies . ix enemyId
  enemy'' = enemy' & damage +~ damage'

investigatorStep :: ArkhamGameStateStep
investigatorStep = ArkhamGameStateStepInvestigatorActionStep

fightOf :: (MonadIO m, HasEnemies a) => a -> UUID -> m Int
fightOf g uuid = pure $ _enemyCombat enemy'
  where enemy' = fromJustNote "could not find enemy" $ g ^? enemies . ix uuid

evadeOf :: (MonadIO m, HasEnemies a) => a -> UUID -> m Int
evadeOf g uuid = pure $ _enemyAgility enemy'
  where enemy' = fromJustNote "could not find enemy" $ g ^? enemies . ix uuid

fightAction
  :: Entity ArkhamGame
  -> ArkhamSkillType
  -> UUID
  -> [Int]
  -> Handler ArkhamGameData
fightAction (Entity gameId game) skillType enemyId cardIndexes = do
  token' <- liftIO $ drawChaosToken game
  checkDifficulty <- fightOf game enemyId
  let
    tokenModifier = tokenToModifier game (game ^. activePlayer) token'
    hand' = game ^. activePlayer . hand
    (commitedCards, remainingCards) =
      over both (map snd) $ partition (\(i, _) -> i `elem` cardIndexes) $ zip
        [0 ..]
        hand'
  modifiedSkillValue <- determineModifiedSkillValue
    skillType
    (game ^. activePlayer)
    commitedCards
    tokenModifier
  let
    newGame =
      game
        & gameStateStep
        %~ revealToken token' checkDifficulty modifiedSkillValue commitedCards
        & activePlayer
        . hand
        .~ remainingCards
        & activePlayer
        . discard
        %~ (commitedCards ++)
  runDB $ updateGame gameId newGame

evadeAction
  :: Entity ArkhamGame
  -> ArkhamSkillType
  -> UUID
  -> [Int]
  -> Handler ArkhamGameData
evadeAction (Entity gameId game) skillType enemyId cardIndexes = do
  token' <- liftIO $ drawChaosToken game
  checkDifficulty <- evadeOf game enemyId
  let tokenModifier = tokenToModifier game (game ^. activePlayer) token'

  let
    hand' = game ^. activePlayer . hand
    (commitedCards, remainingCards) =
      over both (map snd) $ partition (\(i, _) -> i `elem` cardIndexes) $ zip
        [0 ..]
        hand'
  modifiedSkillValue <- determineModifiedSkillValue
    skillType
    (game ^. activePlayer)
    commitedCards
    tokenModifier
  let
    newGame =
      game
        & gameStateStep
        %~ revealToken token' checkDifficulty modifiedSkillValue commitedCards
        & activePlayer
        . hand
        .~ remainingCards
        & activePlayer
        . discard
        %~ (commitedCards ++)
  runDB $ updateGame gameId newGame
