{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Game.Runner where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act
import Arkham.Act.Types (Field (..))
import Arkham.Action qualified as Action
import Arkham.ActiveCost
import Arkham.Agenda
import Arkham.Agenda.Types (Field (..))
import Arkham.Asset
import Arkham.Asset.Types (Asset, AssetAttrs (..), Field (..))
import Arkham.Attack
import Arkham.Campaign.Types hiding (campaign, modifiersL)
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Cost qualified as Cost
import Arkham.DamageEffect
import Arkham.Debug
import Arkham.Deck qualified as Deck
import Arkham.Decklist
import Arkham.Effect
import Arkham.Effect.Types (EffectAttrs (effectFinished))
import Arkham.Effect.Window (EffectWindow (EffectCardResolutionWindow))
import Arkham.Enemy
import Arkham.Enemy.Creation (EnemyCreation (..), EnemyCreationMethod (..))
import Arkham.Enemy.Types (EnemyAttrs (..), Field (..))
import Arkham.Entities
import Arkham.Event
import Arkham.Event.Types
import Arkham.Game.Base
import Arkham.Game.Diff
import Arkham.Game.Helpers hiding (
  EnemyEvade,
  EnemyFight,
  createWindowModifierEffect,
  getSpendableClueCount,
  withModifiers,
 )
import Arkham.Game.Json ()
import Arkham.Game.State
import Arkham.Game.Utils
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Helpers.Customization
import Arkham.Helpers.Enemy (spawnAt)
import Arkham.Helpers.Investigator hiding (investigator, matchTarget)
import Arkham.Helpers.Message hiding (
  EnemyDamage,
  InvestigatorDamage,
  InvestigatorDefeated,
  InvestigatorResigned,
  createEnemy,
 )
import Arkham.History
import Arkham.Id
import Arkham.Investigator (
  becomeYithian,
  lookupInvestigator,
  returnToBody,
 )
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Types (InvestigatorAttrs (..))
import Arkham.Investigator.Types qualified as Investigator
import Arkham.Keyword qualified as Keyword
import Arkham.Location
import Arkham.Location.Types (Field (..), LocationAttrs (..))
import Arkham.Matcher hiding (
  AssetCard,
  AssetDefeated,
  AssetExhausted,
  Discarded,
  DuringTurn,
  EncounterCardSource,
  EnemyAttacks,
  EnemyDefeated,
  EventCard,
  FastPlayerWindow,
  InvestigatorDefeated,
  InvestigatorEliminated,
  LocationCard,
  PlayCard,
  RevealLocation,
  SkillCard,
  StoryCard,
 )
import Arkham.Message qualified as Msg
import Arkham.Movement
import Arkham.Name
import Arkham.Phase
import Arkham.Placement
import Arkham.Placement qualified as Placement
import Arkham.PlayerCard
import Arkham.Projection
import Arkham.Scenario
import Arkham.Scenario.Types hiding (scenario)
import Arkham.Skill
import Arkham.Skill.Types (Field (..), Skill, SkillAttrs (..))
import Arkham.Skill.Types qualified as Skill
import Arkham.SkillTest.Runner
import Arkham.SkillTestResult
import Arkham.Source
import Arkham.Story
import Arkham.Story.Types (Field (..), StoryAttrs (..))
import Arkham.Story.Types qualified as Story
import Arkham.Target
import Arkham.Tarot qualified as Tarot
import Arkham.Timing qualified as Timing
import Arkham.Token qualified as Token
import Arkham.Treachery
import Arkham.Treachery.Types (Field (..), drawnFromL)
import Arkham.Window (Window (..), mkAfter, mkWhen, mkWindow)
import Arkham.Window qualified as Window
import Arkham.Zone qualified as Zone
import Control.Lens (each, itraverseOf, itraversed, non, over, set)
import Control.Monad.State.Strict (evalStateT, get, put)
import Data.Aeson (Result (..))
import Data.Data.Lens (biplate)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.These
import Data.These.Lens
import Data.Typeable

getInvestigatorsInOrder :: HasGame m => m [InvestigatorId]
getInvestigatorsInOrder = do
  g <- getGame
  pure $ g ^. playerOrderL

runGameMessage :: Runner Game
runGameMessage msg g = case msg of
  RemovePlayerCardFromGame addToRemovedFromGame card -> do
    when addToRemovedFromGame $ push $ RemovedFromGame card
    pure g
  SetGameState s -> pure $ g & gameStateL .~ s
  ChoosingDecks -> pure $ g & entitiesL . investigatorsL .~ mempty & gameStateL .~ IsChooseDecks (g ^. playersL)
  DoneChoosingDecks -> pure $ g & gameStateL .~ IsActive
  IncreaseCustomization iid cardCode customization choices -> do
    cards <- select $ OwnedBy (InvestigatorWithId iid) <> basic (CardWithCardCode cardCode)

    cards' <- forMaybeM cards \case
      card@(PlayerCard pc) -> do
        case customizationIndex card customization of
          Nothing -> pure Nothing
          Just i -> do
            let card' =
                  pc
                    { pcCustomizations =
                        IntMap.alter
                          (Just . maybe (1, choices) (second (const choices) . first (+ 1)))
                          i
                          (pcCustomizations pc)
                    }
            replaceCard card.id (PlayerCard card')
            pure $ Just card'
      _ -> pure Nothing

    let swapCard c d = if d.id == c.id then c else d

    pure
      $ g
      & entitiesL
      %~ (\e -> foldr (over biplate . swapCard) e cards')
      & actionRemovedEntitiesL
      %~ (\e -> foldr (over biplate . swapCard) e cards')
      & inHandEntitiesL
      %~ (\e -> foldr (over biplate . swapCard) e cards')
      & inDiscardEntitiesL
      %~ (\e -> foldr (over biplate . swapCard) e cards')
      & inSearchEntitiesL
      %~ (\e -> foldr (over biplate . swapCard) e cards')
      & focusedCardsL
      %~ (\e -> foldr (over biplate . swapCard) e cards')
      & removedFromPlayL
      %~ (\e -> foldr (over biplate . swapCard) e cards')
      & activeCardL
      %~ (\e -> foldr (over biplate . swapCard) e cards')
      & resolvingCardL
      %~ (\e -> foldr (over biplate . swapCard) e cards')
  LoadDecklist playerId decklist -> do
    -- if the player is changing decks during the game (i.e. prologue investigators) we need to replace the old investigator
    let mOldId = toId <$> find ((== playerId) . attr investigatorPlayerId) (toList $ gameInvestigators g)
        replaceIds = InvestigatorId "00000" : toList mOldId

    dl <- loadDecklist decklist
    let iid' = decklistInvestigator dl
    let deck = decklistCards dl
    let sideDeck = decklistExtraDeck dl
    let investigator =
          overAttrs
            ( \ia ->
                ia
                  { investigatorTaboo = decklistTaboo dl
                  , investigatorMutated = tabooMutated' (decklistTaboo dl) (coerce iid')
                  }
            )
            (lookupInvestigator iid' playerId)
    let iid = toId investigator
    when (notNull sideDeck) $ push $ LoadSideDeck iid sideDeck
    push $ InitDeck iid (decklistUrl dl) (Deck deck)
    let activeInvestigatorF =
          if gameActiveInvestigatorId g `elem` replaceIds then set activeInvestigatorIdL iid else id
        turnPlayerInvestigatorF =
          if gameTurnPlayerInvestigatorId g `elem` map Just replaceIds
            then set turnPlayerInvestigatorIdL (Just iid)
            else id
    pure
      $ g
      & ( entitiesL
            . investigatorsL
            %~ insertEntity investigator
            . Map.filter ((/= playerId) . attr investigatorPlayerId)
        )
      & activeInvestigatorF
      & turnPlayerInvestigatorF
  SetInvestigator playerId investigator -> do
    -- if the player is changing decks during the game (i.e. prologue investigators) we need to replace the old investigator
    let mOldId = toId <$> find ((== playerId) . attr investigatorPlayerId) (toList $ gameInvestigators g)
        replaceIds = InvestigatorId "00000" : toList mOldId

    let iid = toId investigator
    let activeInvestigatorF =
          if gameActiveInvestigatorId g `elem` replaceIds then set activeInvestigatorIdL iid else id
        turnPlayerInvestigatorF =
          if gameTurnPlayerInvestigatorId g `elem` map Just replaceIds
            then set turnPlayerInvestigatorIdL (Just iid)
            else id
    pure
      $ g
      & ( entitiesL
            . investigatorsL
            %~ insertEntity investigator
            . Map.filter ((/= playerId) . attr investigatorPlayerId)
        )
      & activeInvestigatorF
      & turnPlayerInvestigatorF
  Run msgs -> g <$ pushAll msgs
  If wType _ -> do
    window <- checkWindows [mkWindow Timing.AtIf wType]
    g <$ pushAll [window, Do msg]
  Do (If _ msgs) -> g <$ pushAll msgs
  IfEnemyExists eMatcher msgs -> do
    whenM (selectAny eMatcher) $ pushAll msgs
    pure g
  BeginAction ->
    pure
      $ g
      & (inActionL .~ True)
      & (actionCanBeUndoneL .~ True)
      & (actionDiffL .~ [])
  FinishAction -> do
    iid <- getActiveInvestigatorId
    let
      historyItem = HistoryItem HistoryActionsCompleted 1
      turn = isJust $ view turnPlayerInvestigatorIdL g
      setTurnHistory =
        if turn then turnHistoryL %~ insertHistory iid historyItem else id
    pure
      $ g
      & (inActionL .~ False)
      & (actionCanBeUndoneL .~ False)
      & (actionDiffL .~ [])
      & (inDiscardEntitiesL .~ mempty)
      & (phaseHistoryL %~ insertHistory iid historyItem)
      & setTurnHistory
  ActionCannotBeUndone -> pure $ g & actionCanBeUndoneL .~ False
  UndoAction -> do
    -- gameActionDiff contains a list of diffs, in order, to revert the game
    -- The gameActionDiff will be empty after this so we do not need the diffs to store any data
    pure $ foldl' unsafePatch g (gameActionDiff g)
  EndOfGame mNextCampaignStep -> do
    window <- checkWindows [mkWhen Window.EndOfGame]
    push window
    pushEnd $ EndOfScenario mNextCampaignStep
    pure g
  EndOfScenario _ -> do
    let
      update g' =
        g'
          & (entitiesL . assetsL .~ mempty)
          & (entitiesL . locationsL .~ mempty)
          & (entitiesL . enemiesL .~ mempty)
          & (entitiesL . actsL .~ mempty)
          & (entitiesL . agendasL .~ mempty)
          & (entitiesL . treacheriesL .~ mempty)
          & (entitiesL . eventsL .~ mempty)
          & (entitiesL . effectsL %~ filterMap (or . sequence [effectIsForResolution, effectIsForNextGame]))
          & (entitiesL . skillsL .~ mempty)
          & (entitiesL . storiesL .~ mempty)
          & (encounterDiscardEntitiesL .~ defaultEntities)
          & (skillTestL .~ Nothing)
          & (skillTestResultsL .~ Nothing)
          & (inDiscardEntitiesL .~ mempty)
          & (inHandEntitiesL .~ mempty)
          & (inSearchEntitiesL .~ mempty)
          & (focusedCardsL .~ mempty)
          & (focusedChaosTokensL .~ mempty)
          & (activeCardL .~ Nothing)
          & (activeAbilitiesL .~ mempty)
          & (actionRemovedEntitiesL .~ mempty)
          & (activeAbilitiesL .~ mempty)
    case gameMode g of
      These c _ -> pure $ update $ g & (modeL .~ This c)
      _ -> pure $ update g
  ResetGame ->
    pure
      $ g
      & (encounterDiscardEntitiesL .~ defaultEntities)
      & (skillTestL .~ Nothing)
      & (skillTestResultsL .~ Nothing)
      & (entitiesL . assetsL .~ mempty)
      & (entitiesL . locationsL .~ mempty)
      & (entitiesL . enemiesL .~ mempty)
      & (entitiesL . actsL .~ mempty)
      & (entitiesL . agendasL .~ mempty)
      & (entitiesL . treacheriesL .~ mempty)
      & (entitiesL . eventsL .~ mempty)
      & (entitiesL . effectsL %~ filterMap effectIsForNextGame)
      & (entitiesL . skillsL .~ mempty)
      & (entitiesL . storiesL .~ mempty)
      & (inDiscardEntitiesL .~ mempty)
      & (inHandEntitiesL .~ mempty)
      & (gameStateL .~ IsActive)
      & (turnPlayerInvestigatorIdL .~ Nothing)
      & (focusedCardsL .~ mempty)
      & (focusedChaosTokensL .~ mempty)
      & (activeCardL .~ Nothing)
      & (activeAbilitiesL .~ mempty)
      & (playerOrderL .~ (g ^. entitiesL . investigatorsL . to keys))
      & (actionRemovedEntitiesL .~ mempty)
      & (activeAbilitiesL .~ mempty)
      & (foundCardsL .~ mempty)
  StartScenario sid -> do
    -- NOTE: The campaign log and player decks need to be copied over for
    -- standalones because we effectively reset it here when we `setScenario`.
    let
      difficulty = these difficultyOf difficultyOfScenario (const . difficultyOf) (g ^. modeL)
      mCampaignLog =
        these (const Nothing) (Just . attr scenarioStandaloneCampaignLog) (\_ _ -> Nothing) (g ^. modeL)
      playerDecks = these (const mempty) (attr scenarioPlayerDecks) (\_ _ -> mempty) (g ^. modeL)
      setCampaignLog = case mCampaignLog of
        Nothing -> id
        Just cl -> overAttrs (standaloneCampaignLogL .~ cl)

      standalone = isNothing $ modeCampaign $ g ^. modeL
      setPlayerDecks = overAttrs (playerDecksL .~ playerDecks)

    clearCardCache

    pushAll
      $ LoadTarotDeck
      : PreScenarioSetup
      : [StandaloneSetup | standalone]
        <> [ChooseLeadInvestigator]
        <> [PerformTarotReading | gamePerformTarotReadings g]
        <> [ SetupInvestigators
           , SetChaosTokensForScenario -- (chaosBagOf campaign')
           , InvestigatorsMulligan
           , Setup
           , EndSetup
           ]
    pure
      $ g
      & (modeL %~ setScenario (setPlayerDecks $ setCampaignLog $ lookupScenario sid difficulty))
      & (phaseL .~ InvestigationPhase)
      & (cardsL %~ filterMap (not . isEncounterCard))
  PerformTarotReading -> do
    lead <- getLeadPlayer
    push
      $ questionLabel "Choose Tarot Reading Type" lead
      $ ChooseOne
        [ Label "Chaos" [PerformReading Tarot.Chaos]
        , Label "Balance" [PerformReading Tarot.Balance]
        , Label "Choice" [PerformReading Tarot.Choice]
        ]
    pure g
  RestartScenario -> do
    let standalone = isNothing $ modeCampaign $ g ^. modeL
    pushAll
      $ ResetGame
      : [StandaloneSetup | standalone]
        <> [ ChooseLeadInvestigator
           , SetupInvestigators
           , SetChaosTokensForScenario -- (chaosBagOf campaign')
           , InvestigatorsMulligan
           , Setup
           , EndSetup
           ]
    pure $ g & (phaseL .~ InvestigationPhase)
  BeginGame -> do
    let (before, _, after) = frame Window.GameBegins
    iids <- getInvestigatorsInOrder
    pushAll $ before : map (`ForInvestigator` BeginGame) iids <> [after]
    pure g
  InvestigatorsMulligan -> do
    iids <- getInvestigatorsInOrder
    g <$ pushAll [InvestigatorMulligan iid | iid <- iids]
  InvestigatorMulligan iid -> pure $ g & activeInvestigatorIdL .~ iid
  Will msg'@(ResolveChaosToken token tokenFace iid) -> do
    mods <- getModifiers iid
    let
      resolutionChoices =
        flip mapMaybe mods \case
          CanResolveToken tokenFace' target | tokenFace == tokenFace' -> Just target
          _ -> Nothing
    whenWindow <- checkWindows [mkWhen (Window.ResolvesChaosToken iid token)]
    if null resolutionChoices
      then pushAll [whenWindow, msg']
      else do
        player <- getPlayer iid
        push
          $ chooseOne player
          $ [ targetLabel target [whenWindow, TargetResolveChaosToken target token tokenFace iid]
            | target <- resolutionChoices
            ]
          <> [Label "Resolve Normally" [whenWindow, msg']]
    pure g
  CreateEffect builder -> do
    (effectId, effect) <- createEffect builder
    push (CreatedEffect effectId effect.metadata effect.source effect.target)
    pure $ g & entitiesL . effectsL %~ insertMap effectId effect
  CreateChaosTokenValueEffect sid n source target -> do
    (effectId, effect) <- createChaosTokenValueEffect sid n source target
    ems <- effectModifiers source [ChaosTokenValueModifier n]
    push $ CreatedEffect effectId (Just ems) source target
    pure $ g & entitiesL . effectsL %~ insertMap effectId effect
  PayCardCost iid card windows' -> do
    activeCost <- createActiveCostForCard iid card NotPlayAction windows'
    -- _ <- error "This is broken because it also plays the card, rethink cards that call this"
    push $ CreatedCost (activeCostId activeCost)
    pure $ g & activeCostL %~ insertMap (activeCostId activeCost) activeCost
  CancelCost acId -> do
    pure $ g & activeCostL %~ deleteMap acId
  PayAdditionalCost iid batchId cost -> do
    acId <- getRandom
    let
      activeCost =
        ActiveCost
          { activeCostId = acId
          , activeCostCosts = cost
          , activeCostPayments = Cost.NoPayment
          , activeCostTarget = ForAdditionalCost batchId
          , activeCostWindows = []
          , activeCostInvestigator = iid
          , activeCostSealedChaosTokens = []
          }
    push $ CreatedCost acId
    pure $ g & activeCostL %~ insertMap acId activeCost
  PayForAbility ability windows' -> do
    acId <- getRandom
    iid <- toId <$> getActiveInvestigator
    -- imods <- getModifiers iid
    modifiers' <- getModifiers (AbilityTarget iid ability)
    -- TODO: we might want to check the ability index and source
    let
      -- isMovement = abilityIs ability #move
      isInvestigate = abilityIs ability #investigate
      isResign = abilityIs ability #resign

    doDelayAdditionalCosts <- case abilityDelayAdditionalCosts ability of
      Nothing -> pure False
      Just delay -> case delay of
        DelayAdditionalCosts -> pure True
        DelayAdditionalCostsWhen c -> passesCriteria iid Nothing ability.source ability.source [] c

    -- leaveCosts <-
    --   if isMovement && not doDelayAdditionalCosts
    --     then do
    --       mlocation <- getMaybeLocation iid
    --       case mlocation of
    --         Nothing -> pure []
    --         Just lid -> do
    --           mods' <- getModifiers lid
    --           pure [c | AdditionalCostToLeave c <- mods']
    --     else pure []

    -- -- TODO: we might care about other sources here
    -- enterCosts <-
    --   if isMovement && not doDelayAdditionalCosts
    --     then case abilitySource ability of
    --       LocationSource lid -> do
    --         mods' <- getModifiers lid
    --         pcosts <- filterM ((lid <=~>) . fst) [(ma, c) | AdditionalCostToEnterMatching ma c <- imods]
    --         pure $ map snd pcosts <> [c | AdditionalCostToEnter c <- mods']
    --       _ -> pure []
    --     else pure []

    investigateCosts <-
      if isInvestigate && not doDelayAdditionalCosts
        then do
          getMaybeLocation iid >>= \case
            Just lid -> do
              mods' <- getModifiers lid
              pure [c | AdditionalCostToInvestigate c <- mods']
            _ -> pure []
        else pure []

    resignCosts <-
      if isResign && not doDelayAdditionalCosts
        then do
          getMaybeLocation iid >>= \case
            Just lid -> do
              mods' <- getModifiers lid
              pure [c | AdditionalCostToResign c <- mods']
            _ -> pure []
        else pure []

    let
      costF =
        case find isSetCost modifiers' of
          Just (SetAbilityCost c) -> const c
          _ -> (`applyCostModifiers` modifiers')
      isSetCost = \case
        SetAbilityCost _ -> True
        _ -> False
      additionalCosts =
        if doDelayAdditionalCosts
          then []
          else
            abilityAdditionalCosts ability <> flip mapMaybe modifiers' \case
              AdditionalCost c -> Just c
              _ -> Nothing
    let
      fixEnemy = maybe id replaceThatEnemy $ getThatEnemy windows'
      activeCost =
        ActiveCost
          { activeCostId = acId
          , activeCostCosts =
              fixEnemy
                $ mconcat
                  ( costF (abilityCost ability)
                      : additionalCosts ++ investigateCosts ++ resignCosts
                  )
          , activeCostPayments = Cost.NoPayment
          , activeCostTarget = ForAbility ability
          , activeCostWindows = windows'
          , activeCostInvestigator = iid
          , activeCostSealedChaosTokens = []
          }
    push $ CreatedCost acId
    pure $ g & activeCostL %~ insertMap acId activeCost
  PayCostFinished acId -> pure $ g & activeCostL %~ deleteMap acId
  CreateWindowModifierEffect effectWindow effectMetadata source target -> do
    (effectId, effect) <-
      createWindowModifierEffect
        effectWindow
        effectMetadata
        source
        target
    push (CreatedEffect effectId (Just effectMetadata) source target)
    pure $ g & entitiesL . effectsL %~ insertMap effectId effect
  CreateChaosTokenEffect effectMetadata source token -> do
    (effectId, effect) <- createChaosTokenEffect effectMetadata source token
    push
      $ CreatedEffect
        effectId
        (Just effectMetadata)
        source
        (ChaosTokenTarget token)
    pure $ g & entitiesL . effectsL %~ insertMap effectId effect
  CreateOnRevealChaosTokenEffect sid matchr source target message -> do
    (effectId, effect) <- createOnRevealChaosTokenEffect sid matchr source target message
    push $ CreatedEffect effectId Nothing source target
    pure $ g & entitiesL . effectsL %~ insertMap effectId effect
  CreateEndOfTurnEffect source iid message -> do
    (effectId, effect) <- createEndOfTurnEffect source iid message
    push $ CreatedEffect effectId Nothing source (toTarget iid)
    pure $ g & entitiesL . effectsL %~ insertMap effectId effect
  CreateEndOfRoundEffect source message -> do
    (effectId, effect) <- createEndOfRoundEffect source message
    push $ CreatedEffect effectId Nothing source GameTarget
    pure $ g & entitiesL . effectsL %~ insertMap effectId effect
  DisableEffect effectId -> do
    effect <- getEffect effectId
    pure
      $ g
      & (entitiesL . effectsL %~ deleteMap effectId)
      & ( actionRemovedEntitiesL
            . effectsL
            %~ insertEntity (overAttrs (\a -> a {effectFinished = True}) effect)
        )
  FocusCards cards -> pure $ g & focusedCardsL .~ cards
  UnfocusCards -> pure $ g & focusedCardsL .~ mempty
  ClearFound FromDeck -> do
    pure $ g & foundCardsL %~ Map.filterWithKey (\k _ -> not (zoneIsFromDeck k))
  ClearFound zone -> pure $ g & foundCardsL . at zone ?~ mempty
  FocusTarotCards cards -> pure $ g & focusedTarotCardsL .~ cards
  UnfocusTarotCards -> pure $ g & focusedTarotCardsL .~ mempty
  PutCardOnTopOfDeck _ _ c -> do
    mSkillId <- selectOne $ SkillWithCardId (toCardId c)
    let skillsF = maybe id deleteMap mSkillId
    pure
      $ g
      & focusedCardsL
      %~ filter (/= c)
      & foundCardsL
      . each
      %~ filter (/= c)
      & entitiesL
      . skillsL
      %~ skillsF
  PutCardOnBottomOfDeck _ _ c -> do
    mSkillId <- selectOne $ SkillWithCardId (toCardId c)
    let skillsF = maybe id deleteMap mSkillId
    pure
      $ g
      & (focusedCardsL %~ filter (/= c))
      & (foundCardsL . each %~ filter (/= c))
      & (entitiesL . skillsL %~ skillsF)
  ShuffleCardsIntoDeck _ cards ->
    pure
      $ g
      & focusedCardsL
      %~ filter (`notElem` cards)
      & foundCardsL
      . each
      %~ filter (`notElem` cards)
  FocusChaosTokens tokens -> pure $ g & focusedChaosTokensL <>~ tokens
  Msg.RevealChaosToken SkillTestSource {} _ token -> pure $ g & focusedChaosTokensL %~ filter (/= token)
  UnfocusChaosTokens -> pure $ g & focusedChaosTokensL .~ mempty
  ChoosePlayer iid SetLeadInvestigator -> do
    players <- getInvestigators
    push $ ChoosePlayerOrder iid (filter (/= iid) players) [iid]
    pure $ g & leadInvestigatorIdL .~ iid & activeInvestigatorIdL .~ iid
  ChoosePlayer iid SetTurnPlayer -> do
    pushAll [BeginTurn iid, After (BeginTurn iid)]
    pure $ g & activeInvestigatorIdL .~ iid & turnPlayerInvestigatorIdL ?~ iid
  MoveTo (moveTarget -> InvestigatorTarget iid) -> do
    let
      historyItem = HistoryItem HistoryMoved True
      turn = isJust $ view turnPlayerInvestigatorIdL g
      setTurnHistory =
        if turn then turnHistoryL %~ insertHistory iid historyItem else id
    pure $ g & (phaseHistoryL %~ insertHistory iid historyItem) & setTurnHistory
  EnemyDefeated eid _ source _ -> do
    attrs <- toAttrs <$> getEnemy eid
    mlid <- field EnemyLocation eid
    miid <- getSourceController source
    lead <- getLead
    -- TODO: This is wrong but history is the way we track if enemies were
    -- defeated for cards like Kerosene (1), we need a history independent of
    -- the iid for cases where we aren't looking at a specific investigator
    enemyHealth <- fieldJust EnemyHealth eid
    let
      iid = fromMaybe lead miid
      placement' = maybe (enemyPlacement attrs) AtLocation mlid
      historyItem =
        HistoryItem
          HistoryEnemiesDefeated
          [ DefeatedEnemyAttrs
              { defeatedEnemyAttrs = attrs {enemyPlacement = placement'}
              , defeatedEnemyHealth = enemyHealth
              }
          ]
      turn = isJust $ view turnPlayerInvestigatorIdL g
      setTurnHistory =
        if turn then turnHistoryL %~ insertHistory iid historyItem else id
    pure $ g & (phaseHistoryL %~ insertHistory iid historyItem) & setTurnHistory
  Successful (Action.Investigate, LocationTarget lid) iid _ _ _ -> do
    let
      historyItem = HistoryItem HistoryLocationsSuccessfullyInvestigated (singleton lid)
      turn = isJust $ view turnPlayerInvestigatorIdL g
      setTurnHistory =
        if turn then turnHistoryL %~ insertHistory iid historyItem else id
    pure $ g & (phaseHistoryL %~ insertHistory iid historyItem) & setTurnHistory
  FoundCards cards -> pure $ g & foundCardsL .~ cards
  ObtainCard cardId -> pure $ g & foundCardsL . each %~ deleteFirstMatch ((== cardId) . toCardId)
  AddFocusedToTopOfDeck iid EncounterDeckTarget cardId ->
    if null (gameFoundCards g)
      then do
        let
          card =
            fromJustNote "missing card"
              $ find ((== cardId) . toCardId) (g ^. focusedCardsL)
          focusedCards = filter ((/= cardId) . toCardId) (g ^. focusedCardsL)
        push $ PutCardOnTopOfDeck iid Deck.EncounterDeck card
        pure $ g & (focusedCardsL .~ focusedCards)
      else do
        let
          card =
            fromJustNote "missing card"
              $ find
                ((== cardId) . toCardId)
                (concat . toList $ g ^. foundCardsL)
          foundCards =
            Map.map (filter ((/= cardId) . toCardId)) (g ^. foundCardsL)
        push $ PutCardOnTopOfDeck iid Deck.EncounterDeck card
        pure $ g & (foundCardsL .~ foundCards)
  GameOver -> do
    clearQueue
    pure $ g & gameStateL .~ IsOver
  PlaceLocation lid card ->
    if isNothing $ g ^. entitiesL . locationsL . at lid
      then do
        let location = lookupLocation (toCardCode card) lid (toCardId card)
        push (PlacedLocation (toName location) (toCardCode card) lid)
        pure $ g & entitiesL . locationsL . at lid ?~ location
      else pure g
  ReplaceLocation lid card replaceStrategy -> do
    -- if replaceStrategy is swap we also want to copy over revealed, all tokens
    location <- getLocation lid
    let
      oldAttrs = toAttrs location
      orKey k v = case lookup k (locationGlobalMeta oldAttrs) of
        Nothing -> v
        Just v' -> case fromJSON v' of
          Success v'' -> v''
          Error _ -> v

      location' =
        flip overAttrs (lookupLocation (toCardCode card) lid (toCardId card))
          $ \attrs -> case replaceStrategy of
            DefaultReplace ->
              attrs
                { locationRevealed = orKey "replacedIsRevealed" (locationRevealed attrs)
                , locationWithoutClues =
                    orKey "replacedIsWithoutClues" (locationWithoutClues attrs) && oldAttrs.clues == 0
                , locationTokens = locationTokens oldAttrs
                , locationCardsUnderneath = locationCardsUnderneath oldAttrs
                }
            Swap ->
              attrs
                { locationTokens = locationTokens oldAttrs
                , locationRevealed = orKey "replacedIsRevealed" (locationRevealed oldAttrs)
                , locationCardsUnderneath = locationCardsUnderneath oldAttrs
                , locationWithoutClues =
                    orKey "replacedIsWithoutClues" (locationWithoutClues oldAttrs) && oldAttrs.clues == 0
                }
    -- todo: should we just run this in place?
    enemies <- select $ enemyAt lid
    if replaceStrategy == Swap
      then pushAll $ map EnemyCheckEngagement enemies
      else
        pushAll
          $ [PlacedLocation (toName card) (toCardCode card) lid]
          <> map EnemyCheckEngagement enemies
    pure $ g & entitiesL . locationsL . at lid ?~ location'
  ReplaceEnemy eid card replaceStrategy -> do
    -- if replaceStrategy is swap we also want to copy over revealed, all tokens
    enemy <- getEnemy eid
    let
      oldAttrs = toAttrs enemy
      enemy' =
        flip overAttrs (lookupEnemy (toCardCode card) eid (toCardId card))
          $ \attrs -> case replaceStrategy of
            DefaultReplace -> attrs
            Swap ->
              attrs
                { enemyTokens = enemyTokens oldAttrs
                , enemyPlacement = enemyPlacement oldAttrs
                , enemyAssignedDamage = enemyAssignedDamage oldAttrs
                , enemyExhausted = enemyExhausted oldAttrs
                , enemyMovedFromHunterKeyword = enemyMovedFromHunterKeyword oldAttrs
                , enemySealedChaosTokens = enemySealedChaosTokens oldAttrs
                , enemyKeys = enemyKeys oldAttrs
                , enemySpawnedBy = enemySpawnedBy oldAttrs
                , enemyDiscardedBy = enemyDiscardedBy oldAttrs
                }

    pushWhen (replaceStrategy == DefaultReplace)
      $ EnemyCheckEngagement eid
    -- todo: should we just run this in place?
    pure $ g & entitiesL . enemiesL . at eid ?~ enemy'
  RemoveAsset aid -> do
    removedEntitiesF <-
      if notNull (gameActiveAbilities g)
        then do
          asset <- getAsset aid
          pure $ actionRemovedEntitiesL . assetsL %~ insertEntity asset
        else pure id
    pure $ g & entitiesL . assetsL %~ deleteMap aid & removedEntitiesF
  RemoveEvent eid -> do
    popMessageMatching_ $ \case
      Discard _ _ (EventTarget eid') -> eid == eid'
      _ -> False
    removedEntitiesF <-
      if notNull (gameActiveAbilities g)
        then do
          event' <- getEvent eid
          pure $ actionRemovedEntitiesL . eventsL %~ insertEntity event'
        else pure id
    pure $ g & entitiesL . eventsL %~ deleteMap eid & removedEntitiesF
  RemoveEnemy eid -> do
    popMessageMatching_ $ \case
      EnemyDefeated eid' _ _ _ -> eid == eid'
      _ -> False
    popMessageMatching_ $ \case
      Discard _ _ (EnemyTarget eid') -> eid == eid'
      _ -> False
    mEnemy <- maybeEnemy eid
    -- enemy might already be gone (i.e. placed in void)
    case mEnemy of
      Nothing -> pure g
      Just enemy -> do
        swarms <- select $ SwarmOf eid

        case attr enemyPlacement enemy of
          AsSwarm _ c -> case toCardOwner c of
            Just owner -> push $ PutCardOnBottomOfDeck owner (Deck.InvestigatorDeck owner) c
            Nothing -> error "Missing owner"
          _ -> do
            pushAll $ map RemoveEnemy swarms

        pure
          $ g
          & entitiesL
          . enemiesL
          . ix eid
          %~ overAttrs (\x -> x {enemyPlacement = OutOfPlay RemovedZone})
  RemoveSkill sid -> do
    removedEntitiesF <-
      if notNull (gameActiveAbilities g)
        then do
          skill <- getSkill sid
          pure
            $ actionRemovedEntitiesL
            . skillsL
            %~ insertEntity (overAttrs (Skill.placementL .~ OutOfPlay RemovedZone) skill)
        else pure id
    pure $ g & entitiesL . skillsL %~ deleteMap sid & removedEntitiesF
  When (RemoveEnemy enemy) -> do
    pushM $ checkWindows [mkWhen (Window.LeavePlay $ toTarget enemy)]
    pure g
  RemoveTreachery tid -> do
    popMessageMatching_ $ \case
      After (Revelation _ source) -> source == TreacherySource tid
      _ -> False
    removedEntitiesF <-
      if gameInAction g
        then do
          treachery <- getTreachery tid
          pure $ actionRemovedEntitiesL . treacheriesL %~ insertEntity treachery
        else pure id

    pure $ g & entitiesL . treacheriesL %~ deleteMap tid & removedEntitiesF
  When (RemoveLocation lid) -> do
    pushM $ checkWindows [mkWhen (Window.LeavePlay $ toTarget lid)]
    pure g
  RemovedLocation lid -> do
    push $ Do msg
    treacheries <- select $ TreacheryAt $ LocationWithId lid
    pushAll $ concatMap (resolve . toDiscard GameSource) treacheries
    enemies <- select $ enemyAt lid
    pushAll $ concatMap (resolve . toDiscard GameSource) enemies
    events <- select $ eventAt lid
    pushAll $ concatMap (resolve . toDiscard GameSource) events
    assets <- select $ assetAt lid
    pushAll $ concatMap (resolve . toDiscard GameSource) assets
    investigators <- select $ investigatorAt lid
    -- since we handle the would be defeated window in the previous message we
    -- skip directly to the is defeated message even though we would normally
    -- not want to do this
    pushAll $ concatMap (resolve . Msg.InvestigatorIsDefeated (toSource lid)) investigators
    pure g
  Do (RemovedLocation lid) -> pure $ g & entitiesL . locationsL %~ deleteMap lid
  SpendClues 0 _ -> pure g
  SpendClues n iids -> do
    investigatorsWithClues <-
      filter ((> 0) . snd)
        <$> for
          ( filter ((`elem` iids) . fst)
              $ mapToList
              $ g
              ^. entitiesL
              . investigatorsL
          )
          (\(iid, i) -> (iid,) <$> getSpendableClueCount (toAttrs i))
    case investigatorsWithClues of
      [] -> error "someone needed to spend some clues"
      [(x, _)] -> push $ InvestigatorSpendClues x n
      xs -> do
        if sum (map snd investigatorsWithClues) == n
          then
            pushAll
              $ map (uncurry InvestigatorSpendClues) investigatorsWithClues
          else do
            player <- getPlayer (gameLeadInvestigatorId g)
            pushAll
              [ chooseOne player
                  $ map (\(i, _) -> targetLabel i [InvestigatorSpendClues i 1]) xs
              , SpendClues (n - 1) (map fst investigatorsWithClues)
              ]
    pure g
  AdvanceCurrentAgenda -> do
    let aids = keys $ g ^. entitiesL . agendasL
    g <$ pushAll [AdvanceAgenda aid | aid <- aids]
  ReplaceAgenda aid1 card -> do
    agendaDeckId <- field AgendaDeckId aid1
    let
      newAgendaId = AgendaId (toCardCode card)
      newAgenda = lookupAgenda newAgendaId agendaDeckId (toCardId card)

    let (before, _, after) = frame (Window.EnterPlay $ toTarget newAgenda)
    pushAll [before, after]
    pure
      $ g
      & (entitiesL . agendasL %~ insertMap newAgendaId newAgenda . deleteMap aid1)
  ReplaceAct aid1 card -> do
    actDeckId <- fromMaybe 1 <$> fieldMay ActDeckId aid1
    let newActId = ActId (toCardCode card)
    let newAct = either throw id $ lookupAct newActId actDeckId (toCardId card)
    pure
      $ g
      & (entitiesL . actsL %~ insertMap newActId newAct . deleteMap aid1)
  AddAct deckNum card -> do
    let aid = ActId $ toCardCode card
    pure $ g & entitiesL . actsL . at aid ?~ either throw id (lookupAct aid deckNum $ toCardId card)
  AddAgenda agendaDeckNum card -> do
    let aid = AgendaId $ toCardCode card
    pure $ g & entitiesL . agendasL . at aid ?~ lookupAgenda aid agendaDeckNum (toCardId card)
  ReassignHorror source target n -> do
    replaceWindowMany
      \case
        Window.PlacedToken _ t Token.Horror _ -> t == sourceToTarget source
        _ -> False
      \case
        Window.PlacedToken s t Token.Horror m
          | m > n -> [Window.PlacedToken s t Token.Horror (m - n), Window.PlacedToken s target Token.Horror n]
        Window.PlacedToken s _ Token.Horror _ -> [Window.PlacedToken s target Token.Horror n]
        _ -> error "impossible"
    pure g
  CommitCard iid card -> do
    push $ InvestigatorCommittedCard iid card
    case card of
      PlayerCard pc -> case toCardType pc of
        SkillType -> do
          skillId <- getRandom
          let hasInDiscardEffects = cdCardInDiscardEffects (toCardDef card)
          inDiscard <- selectAny $ inDiscardOf iid <> basic (CardWithId card.id)

          let setPlacement =
                overAttrs
                  ( \attrs ->
                      attrs {skillPlacement = if hasInDiscardEffects && inDiscard then StillInDiscard iid else Unplaced}
                  )
          let skill = setPlacement $ createSkill pc iid skillId
          push $ InvestigatorCommittedSkill iid skillId
          for_ (skillAdditionalCost $ toAttrs skill) $ \cost -> do
            let ability = abilityEffect skill [] cost
            push $ PayForAbility ability []
          pure $ g & entitiesL . skillsL %~ insertMap skillId skill
        _ -> pure g
      _ -> pure g
  SkillTestResults resultsData -> pure $ g & skillTestResultsL ?~ resultsData
  Do (SkillTestEnds _ iid _) -> do
    let result = skillTestResult <$> g ^. skillTestL
    let
      resultF =
        case result of
          Just SucceededBy {} -> \case
            IfSuccessfulModifier m -> m
            other -> other
          Just FailedBy {} -> \case
            IfFailureModifier m -> m
            other -> other
          _ -> id

    skills' <-
      map (overAttrs (Skill.placementL .~ OutOfPlay RemovedZone))
        <$> filterMapM (fieldMap SkillPlacement (== Limbo) . toId) (g ^. entitiesL . skillsL)

    skillPairs <- for (mapToList skills') $ \(skillId, skill) -> do
      card <- field SkillCard skillId
      mods <- map resultF <$> liftA2 (<>) (getModifiers skillId) (getModifiers $ toCardId card)
      let
        modifyAfterPlay cur = \case
          SetAfterPlay n -> case cur of
            DevourThis {} -> cur
            _ -> n
          _ -> cur

        afterPlay = foldl' modifyAfterPlay (skillAfterPlay $ toAttrs skill) mods
      pure
        $ if
          | DevourThis iid' <- afterPlay ->
              (Run [ObtainCard (toCard skill).id, Devoured iid' (toCard skill)], Nothing)
          | ReturnToHandAfterTest `elem` mods ->
              ( ReturnToHand (skillOwner $ toAttrs skill) (SkillTarget skillId)
              , Nothing
              )
          | PlaceOnBottomOfDeckInsteadOfDiscard `elem` mods ->
              ( PutCardOnBottomOfDeck
                  (skillOwner $ toAttrs skill)
                  (Deck.InvestigatorDeck $ skillOwner $ toAttrs skill)
                  (toCard skill)
              , Just skillId
              )
          | LeaveCardWhereItIs `elem` mods ->
              (Run [], Just skillId)
          | otherwise -> case afterPlay of
              DiscardThis -> case toCard skill of
                PlayerCard pc ->
                  ( AddToDiscard (skillOwner $ toAttrs skill) pc
                  , Just skillId
                  )
                _ -> error "Unhandled encounter card skill"
              ExileThis -> case toCard skill of
                PlayerCard _ ->
                  ( Exile (toTarget skillId)
                  , Just skillId
                  )
                _ -> error "Unhandled encounter card skill"
              RemoveThisFromGame ->
                (RemoveFromGame (SkillTarget skillId), Nothing)
              AbsoluteRemoveThisFromGame ->
                (RemoveFromGame (SkillTarget skillId), Nothing)
              PlaceThisBeneath target -> (Msg.PlaceUnderneath target [toCard skill], Nothing)
              ReturnThisToHand ->
                (ReturnToHand (skillOwner $ toAttrs skill) (SkillTarget skillId), Nothing)
              ShuffleThisBackIntoDeck ->
                ( ShuffleIntoDeck (Deck.InvestigatorDeck $ skillOwner $ toAttrs skill) (toTarget skill)
                , Just skillId
                )

    pushAll $ map fst skillPairs

    let
      skillTypes = case skillTestType <$> g ^. skillTestL of
        Just (SkillSkillTest skillType) -> [skillType]
        Just (AndSkillTest types) -> types
        Just ResourceSkillTest -> []
        Just BaseValueSkillTest {} -> []
        Nothing -> []
      skillsToRemove = mapMaybe snd skillPairs
      historyItem =
        HistoryItem
          HistorySkillTestsPerformed
          [(skillTypes, fromMaybe Unrun $ skillTestResult <$> g ^. skillTestL)]
      turn = isJust $ view turnPlayerInvestigatorIdL g
      setTurnHistory =
        if turn then turnHistoryL %~ insertHistory iid historyItem else id
    pure
      $ g
      & ( entitiesL
            . skillsL
            %~ Map.filterWithKey
              (\k _ -> k `notElem` skillsToRemove)
        )
      & (phaseHistoryL %~ insertHistory iid historyItem)
      & (actionRemovedEntitiesL . skillsL %~ Map.foldr' (\s m -> Map.insert s.id s m) skills')
      & setTurnHistory
  Msg.SkillTestEnded _ -> do
    pure
      $ g
      & (skillTestL .~ Nothing)
      & (skillTestResultsL .~ Nothing)
  Do msg'@(Search {}) -> do
    inSearch <- fromQueue (elem FinishedSearch)
    if inSearch
      then insertAfterMatching [msg', FinishedSearch] (== FinishedSearch)
      else pushAll [msg', FinishedSearch]
    pure g
  EndSearch iid _ EncounterDeckTarget cardSources -> do
    let
      foundKey = \case
        Zone.FromTopOfDeck _ -> Zone.FromDeck
        Zone.FromBottomOfDeck _ -> Zone.FromDeck
        other -> other
      foundCards = gameFoundCards g
    player <- getPlayer iid

    flip evalStateT True $ do
      for_ cardSources $ \(cardSource, returnStrategy) -> case returnStrategy of
        DiscardRest -> do
          lift
            $ push
            $ chooseOneAtATime player
            $ map
              ( \case
                  EncounterCard c ->
                    TargetLabel
                      (CardIdTarget $ toCardId c)
                      [AddToEncounterDiscard c]
                  _ -> error "not possible"
              )
              (findWithDefault [] Zone.FromDeck foundCards)
        PutBackInAnyOrder -> do
          when (foundKey cardSource /= Zone.FromDeck) $ error "Expects a deck: Game<PutBackInAnyOrder>"
          lift
            $ push
            $ chooseOneAtATime player
            $ map
              (\c -> targetLabel c [AddFocusedToTopOfDeck iid EncounterDeckTarget c.id])
              (findWithDefault [] Zone.FromDeck foundCards)
        ShuffleBackIn -> lift $ pushAll [ClearFound $ foundKey cardSource, ShuffleDeck Deck.EncounterDeck]
        PutBack -> do
          needsContinue <- get
          if needsContinue
            then do
              put False
              lift $ push $ chooseOne player [Label "Continue" [ClearFound $ foundKey cardSource]] -- we don't remove anymore so nothing to do
            else lift $ push (ClearFound $ foundKey cardSource) -- we don't remove anymore so nothing to do
        RemoveRestFromGame -> do
          -- Try to obtain, then don't add back
          lift $ pushAll $ map (ObtainCard . toCardId) $ findWithDefault [] Zone.FromDeck foundCards

    pure g
  FinishedSearch -> do
    pure $ g & foundCardsL .~ mempty
  DiscardedCard cardId -> do
    let
      handleCard card = case card of
        PlayerCard pc -> case pc.owner of
          Just iid -> pushAll [ObtainCard card.id, AddToDiscard iid pc]
          Nothing -> push $ ObtainCard card.id
        EncounterCard ec -> pushAll [ObtainCard card.id, AddToEncounterDiscard ec]
        VengeanceCard vc -> handleCard vc

    handleCard =<< getCard cardId
    pure g
  AddToEncounterDiscard card -> do
    pure
      $ g
      & (focusedCardsL %~ filter (/= EncounterCard card))
      . (foundCardsL . each %~ filter (/= EncounterCard card))
  ReturnToHand iid (SkillTarget skillId) -> do
    card <- field SkillCard skillId
    pushAll [RemoveFromPlay (toSource skillId), addToHand iid card]
    pure g
  ReturnToHand iid (CardIdTarget cardId) -> do
    -- We need to check skills specifically as they aren't covered by the skill
    -- test runner
    mSkill <- selectOne $ SkillWithCardId cardId
    case mSkill of
      Just skillId -> do
        card <- field SkillCard skillId
        push $ addToHand iid card
        pure $ g & entitiesL . skillsL %~ deleteMap skillId
      Nothing -> pure g
  ReturnToHand iid (AssetTarget assetId) -> do
    -- If we try to return to hand but the asset is gone, then do nothing
    mAsset <- maybeAsset assetId
    for_ mAsset $ \asset -> do
      removeAllMessagesMatching $ \case
        Discarded (AssetTarget assetId') _ _ -> assetId == assetId'
        _ -> False

      card <- field AssetCard assetId
      if assetIsStory $ toAttrs asset
        then push $ toDiscard GameSource $ toTarget assetId
        else pushAll [RemoveFromPlay (toSource assetId), addToHand iid card]
    pure g
  PlaceEnemy enemyId placement | not (isOutOfPlayZonePlacement placement) -> do
    enemy <- getEnemy enemyId
    when (isOutOfPlayPlacement $ attr enemyPlacement enemy) do
      case placement of
        AtLocation lid ->
          pushAll
            $ [ Will (EnemySpawn Nothing lid enemyId)
              , When (EnemySpawn Nothing lid enemyId)
              , EnemySpawn Nothing lid enemyId
              , After (EnemySpawn Nothing lid enemyId)
              ]
        _ -> pure ()
    pure g
  PlaceInBonded _ (toCardId -> cardId) -> do
    assets <- select $ AssetWithCardId cardId
    events <- select $ EventWithCardId cardId
    skills <- select $ SkillWithCardId cardId
    enemies <- select $ EnemyWithCardId cardId
    treacheries <- select $ TreacheryWithCardId cardId
    pushAll $ map (RemovedFromPlay . AssetSource) assets
    pushAll $ map (RemovedFromPlay . EventSource) events
    pushAll $ map (RemovedFromPlay . SkillSource) skills
    pushAll $ map (RemovedFromPlay . EnemySource) enemies
    pushAll $ map (RemovedFromPlay . TreacherySource) treacheries
    pure g
  RemovedFromPlay (AssetSource assetId) -> do
    runMessage (RemoveAsset assetId) g
  RemovedFromPlay (EventSource eventId) -> do
    runMessage (RemoveEvent eventId) g
  RemovedFromPlay (SkillSource skillId) -> do
    runMessage (RemoveSkill skillId) g
  RemovedFromPlay (EnemySource enemyId) -> do
    runMessage (RemoveEnemy enemyId) g
  RemovedFromPlay (TreacherySource treacheryId) -> do
    runMessage (RemoveTreachery treacheryId) g
  ReturnToHand iid (EventTarget eventId) -> do
    card <- field EventCard eventId
    push $ addToHand iid card
    pure $ g & entitiesL . eventsL %~ deleteMap eventId
  After (ShuffleIntoDeck _ (AssetTarget aid)) -> do
    runMessage (RemoveAsset aid) g
  After (ShuffleIntoDeck _ (EventTarget eid)) ->
    pure $ g & entitiesL . eventsL %~ deleteMap eid
  ShuffleIntoDeck deck (TreacheryTarget treacheryId) -> do
    treachery <- getTreachery treacheryId
    adjustedDeck <- case deck of
      Deck.InvestigatorDeck _ ->
        maybe Deck.EncounterDeck Deck.InvestigatorDeck <$> field TreacheryOwner treacheryId
      _ -> pure deck

    pushAll
      [ RemoveTreachery treacheryId
      , ShuffleCardsIntoDeck adjustedDeck [toCard treachery]
      ]
    pure g
  ShuffleIntoDeck deck (EnemyTarget enemyId) -> do
    -- The Thing That Follows
    card <- field EnemyCard enemyId
    push $ ShuffleCardsIntoDeck deck [card]
    pure $ g & entitiesL . enemiesL %~ deleteMap enemyId
  ShuffleIntoDeck deck (LocationTarget locationId) -> do
    -- The Thing That Follows
    card <- field LocationCard locationId
    push $ ShuffleCardsIntoDeck deck [card]
    pure $ g & entitiesL . locationsL %~ deleteMap locationId
  PlayCard iid card _mtarget _payment windows' True -> do
    allModifiers <- mconcat <$> sequence [getModifiers card, getModifiers iid]
    let
      isFast = case card of
        PlayerCard _ ->
          isJust $ cdFastWindow (toCardDef card) <|> listToMaybe [w | BecomesFast w <- allModifiers]
        _ -> False
      isPlayAction = if isFast then NotPlayAction else IsPlayAction
    activeCost <- createActiveCostForCard iid card isPlayAction windows'

    let historyItem = HistoryItem HistoryPlayedCards [card]
        turn = isJust $ view turnPlayerInvestigatorIdL g
        setTurnHistory = if turn then turnHistoryL %~ insertHistory iid historyItem else id

    push $ CreatedCost $ activeCostId activeCost
    pure
      $ g
      & activeCostL
      %~ insertMap (activeCostId activeCost) activeCost
      & (phaseHistoryL %~ insertHistory iid historyItem)
      & setTurnHistory
  WindowAsk ws pid q -> do
    -- get all other asks for these windows and combine into an AskMap
    others <- popMessagesMatching \case
      WindowAsk ws' _ _ -> ws == ws'
      _ -> False

    pushAll
      $ ( if notNull others
            then AskMap $ Map.fromList $ (pid, q) : [(pid', q') | WindowAsk _ pid' q' <- others]
            else Ask pid q
        )
      : [Do (CheckWindows ws)]

    pure g
  PlayCard iid card mtarget payment windows' False -> do
    investigator' <- getInvestigator iid
    playableCards <- getPlayableCards (toAttrs investigator') Cost.PaidCost windows'
    case find (== card) playableCards of
      Nothing -> do
        debugOut InfoLevel
          $ "Tried to play "
          <> tshow card
          <> " but it is not in the list of playable cards"
        pure g
      Just _ -> do
        mods <- getModifiers iid
        cardMods <- getModifiers (CardIdTarget $ toCardId card)
        let owner = fromMaybe iid $ listToMaybe [o | PlayableCardOf o c <- mods, c == card]
        let controller = fromMaybe owner $ listToMaybe [c | PlayUnderControlOf c <- cardMods]
        send $ format investigator' <> " played " <> format card
        g' <- runGameMessage (PutCardIntoPlay controller card mtarget payment windows') g
        let
          recordLimit g'' = \case
            MaxPerGame _ -> g'' & cardUsesL . at (toCardCode card) . non 0 +~ 1
            MaxPerRound _ -> g'' & cardUsesL . at (toCardCode card) . non 0 +~ 1
            MaxPerTraitPerRound _ _ -> g'' & cardUsesL . at (toCardCode card) . non 0 +~ 1
            _ -> g''
        pure $ foldl' recordLimit g' (cdLimits $ toCardDef card)
  PutCardIntoPlay iid card mtarget payment windows' -> do
    let cardId = toCardId card
    case card of
      PlayerCard pc -> case toCardType pc of
        PlayerTreacheryType -> do
          tid <- getRandom
          let treachery = lookupTreachery (toCardCode pc) iid tid cardId
          let isPermanent = cdPermanent $ toCardDef treachery
          if isPermanent
            then do
              pushAll [CardEnteredPlay iid card, PlaceTreachery tid (Placement.InThreatArea iid)]
              pure $ g & (entitiesL . treacheriesL %~ insertMap tid treachery)
            else do
              modifiers' <- getCombinedModifiers [TreacheryTarget tid, CardIdTarget cardId]
              let ignoreRevelation = IgnoreRevelation `elem` modifiers'
              let revelation = Revelation iid (TreacherySource tid)
              pushAll
                $ CardEnteredPlay iid card
                : ( guard (not ignoreRevelation) *> [When revelation, revelation, MoveWithSkillTest (After revelation)]
                  )
                  <> [UnsetActiveCard]
              pure
                $ g
                & (entitiesL . treacheriesL %~ insertMap tid treachery)
                & (activeCardL ?~ card)
        AssetType -> do
          -- asset might have been put into play via revelation
          mAid <- selectOne $ AssetWithCardId cardId
          aid <- maybe getRandom pure mAid
          -- We need to start the placement as in play area so that CardEnteredPlay triggers only once
          asset <-
            overAttrs (\attrs -> attrs {assetController = Just iid})
              <$> runMessage
                (SetOriginalCardCode $ pcOriginalCardCode pc)
                (createAsset card aid)
          pushAll
            [ PaidForCardCost iid card payment
            , CardIsEnteringPlay iid card
            , InvestigatorPlayAsset iid aid
            , ResolvedCard iid card
            ]
          pure $ g & entitiesL . assetsL %~ insertMap aid asset
        EventType -> do
          investigator' <- getInvestigator iid
          let
            zone =
              if card `elem` investigatorHand (toAttrs investigator')
                then Zone.FromHand
                else Zone.FromDiscard
          eid <- getRandom
          let
            event' =
              flip overAttrs (createEvent pc iid eid) \attrs ->
                attrs
                  { eventWindows = windows'
                  , eventPlayedFrom = zone
                  , eventTarget = mtarget
                  , eventOriginalCardCode = pcOriginalCardCode pc
                  , eventPayment = payment
                  , eventPlacement = Limbo
                  }

          pushAll
            [ CardEnteredPlay iid card
            , InvestigatorPlayEvent iid eid mtarget windows' zone
            , FinishedEvent eid
            , ResolvedCard iid card
            ]
          pure $ g & entitiesL . eventsL %~ insertMap eid event'
        _ -> pure g
      EncounterCard ec -> case toCardType ec of
        TreacheryType -> do
          tid <- getRandom
          let treachery = createTreachery card iid tid
          pushAll [CardEnteredPlay iid card, PlaceTreachery tid (InThreatArea iid)]
          pure $ g & (entitiesL . treacheriesL %~ insertMap tid treachery)
        EncounterAssetType -> do
          -- asset might have been put into play via revelation
          mAid <- selectOne $ AssetWithCardId cardId
          aid <- maybe getRandom pure mAid
          let asset = overAttrs (\attrs -> attrs {assetController = Just iid}) $ createAsset card aid
          pushAll
            [ InvestigatorPlayAsset iid aid
            , ResolvedCard iid card
            ]
          pure $ g & entitiesL . assetsL %~ insertMap aid asset
        _ -> pure g
      VengeanceCard _ -> error "Vengeance card"
  DrewPlayerEnemy iid card -> do
    investigator <- getInvestigator iid
    send $ format investigator <> " drew " <> format card
    sendEnemy (toTitle investigator <> " drew Enemy") (toJSON card)
    enemyId <- getRandom
    let enemy = overAttrs (\e -> e {enemyBearer = Just iid}) (createEnemy card enemyId)
    pushAll
      [ RemoveCardFromHand iid (toCardId card)
      , InvestigatorDrawEnemy iid enemyId
      ]
    pure $ g & entitiesL . enemiesL %~ insertMap enemyId enemy & resolvingCardL ?~ card
  Would _ [] -> pure $ g & currentBatchIdL .~ Nothing
  Would bId (x : xs) -> do
    pushAll [x, Would bId xs]
    pure $ g & currentBatchIdL ?~ bId
  ExcessDamage _ msgs -> do
    pushAll msgs
    pure g
  CancelBatch bId -> do
    withQueue_ $ \q ->
      flip map q $ \case
        CheckWindows ws -> CheckWindows (filter ((/= Just bId) . windowBatchId) ws)
        Do (CheckWindows ws) -> Do (CheckWindows (filter ((/= Just bId) . windowBatchId) ws))
        other -> other
    removeAllMessagesMatching $ \case
      Would bId' _ -> bId == bId'
      DoBatch bId' _ -> bId == bId'
      CheckWindows [] -> True
      Do (CheckWindows []) -> True
      _ -> False
    pure g
  IgnoreBatch bId -> do
    removeAllMessagesMatching $ \case
      Would bId' _ -> bId == bId'
      DoBatch bId' _ -> bId == bId'
      _ -> False
    pure g
  DoBatch _ msg'@(Discarded {}) -> do
    push msg'
    pure g
  DoBatch _ (Run msgs) -> do
    pushAll msgs
    pure g
  CancelEachNext source msgTypes -> do
    push
      =<< checkWindows
        [mkAfter (Window.CancelledOrIgnoredCardOrGameEffect source)]
    for_ msgTypes $ \msgType -> do
      mRemovedMsg <- withQueue $ \queue ->
        let
          (before, after) = break ((== Just msgType) . messageType) queue
          (remaining, removed) = case after of
            [] -> ([], Nothing)
            (x : xs) -> (xs, Just x)
         in
          (before <> remaining, removed)

      for mRemovedMsg $ \removedMsg -> do
        case removedMsg of
          InvestigatorDrawEnemy iid' eid -> do
            pushAll [toDiscardBy iid' GameSource (EnemyTarget eid), UnsetActiveCard]
          Revelation iid' source' -> do
            removeAllMessagesMatchingM $ \case
              When whenMsg -> pure $ removedMsg == whenMsg
              AfterRevelation iid'' tid ->
                pure $ iid' == iid'' && TreacherySource tid == source'
              CheckWindows wins -> do
                let
                  isRevelationDrawCard = \case
                    (windowType -> Window.DrawCard _ c _) -> (== c) <$> sourceToCard source'
                    _ -> pure False
                anyM isRevelationDrawCard wins
              Do (CheckWindows wins) -> do
                let
                  isRevelationDrawCard = \case
                    (windowType -> Window.DrawCard _ c _) -> (== c) <$> sourceToCard source'
                    _ -> pure False
                anyM isRevelationDrawCard wins
              _ -> pure False
            case source' of
              TreacherySource tid ->
                replaceMessage
                  (After removedMsg)
                  [toDiscardBy iid' GameSource (TreacheryTarget tid), UnsetActiveCard]
              _ -> pure ()
          _ -> pure ()
    pure g
  SkillTestAsk (Ask iid1 (ChooseOne c1)) -> do
    mNextMessage <- peekMessage
    case mNextMessage of
      Just (SkillTestAsk (Ask iid2 (ChooseOne c2))) -> do
        _ <- popMessage
        push
          $ SkillTestAsk
          $ AskMap
          $ mapFromList
            [(iid1, ChooseOne c1), (iid2, ChooseOne c2)]
      _ -> push (chooseOne iid1 c1)
    pure g
  SkillTestAsk (AskMap askMap) -> do
    mNextMessage <- peekMessage
    case mNextMessage of
      Just (SkillTestAsk (Ask iid2 (ChooseOne c2))) -> do
        _ <- popMessage
        push
          $ SkillTestAsk
          $ AskMap
          $ insertWith
            ( \x y -> case (x, y) of
                (ChooseOne m, ChooseOne n) -> ChooseOne $ m <> n
                _ -> error "unhandled"
            )
            iid2
            (ChooseOne c2)
            askMap
      _ -> push $ AskMap askMap
    pure g
  AskPlayer (Ask iid1 (ChooseOne c1)) -> do
    mNextMessage <- peekMessage
    case mNextMessage of
      Just (AskPlayer (Ask iid2 (ChooseOne c2))) -> do
        _ <- popMessage
        push
          $ AskPlayer
          $ AskMap
          $ mapFromList
            [(iid1, ChooseOne c1), (iid2, ChooseOne c2)]
      _ -> push (chooseOne iid1 c1)
    pure g
  AskPlayer (Ask iid1 (PlayerWindowChooseOne c1)) -> do
    mNextMessage <- peekMessage
    case mNextMessage of
      Just (AskPlayer (Ask iid2 (PlayerWindowChooseOne c2))) -> do
        _ <- popMessage
        push
          $ AskPlayer
          $ AskMap
          $ mapFromList
            [(iid1, PlayerWindowChooseOne c1), (iid2, PlayerWindowChooseOne c2)]
      _ -> push (Ask iid1 $ PlayerWindowChooseOne c1)
    pure g
  AskPlayer (AskMap askMap) -> do
    mNextMessage <- peekMessage
    case mNextMessage of
      Just (AskPlayer (Ask iid2 (ChooseOne c2))) -> do
        _ <- popMessage
        push
          $ AskPlayer
          $ AskMap
          $ insertWith
            ( \x y -> case (x, y) of
                (ChooseOne m, ChooseOne n) -> ChooseOne $ m <> n
                _ -> error "unhandled"
            )
            iid2
            (ChooseOne c2)
            askMap
      Just (AskPlayer (Ask iid2 (PlayerWindowChooseOne c2))) -> do
        _ <- popMessage
        push
          $ AskPlayer
          $ AskMap
          $ insertWith
            ( \x y -> case (x, y) of
                (PlayerWindowChooseOne m, PlayerWindowChooseOne n) -> PlayerWindowChooseOne $ m <> n
                _ -> error "unhandled"
            )
            iid2
            (PlayerWindowChooseOne c2)
            askMap
      _ -> push $ AskMap askMap
    pure g
  EnemyWillAttack details -> do
    modifiers' <- getModifiers (attackTarget details)
    cannotBeAttacked <- flip anyM modifiers' $ \case
      CannotBeAttackedBy matcher ->
        elem (attackEnemy details) <$> select matcher
      _ -> pure False
    if not cannotBeAttacked
      then do
        mNextMessage <- peekMessage
        case mNextMessage of
          Just (EnemyAttacks as) -> do
            _ <- popMessage
            push $ EnemyAttacks (EnemyAttack details : as)
          Just aoo@(CheckAttackOfOpportunity _ _) -> do
            _ <- popMessage
            pushAll [aoo, msg]
          Just (EnemyWillAttack details2) -> do
            _ <- popMessage
            modifiers2' <- getModifiers (attackTarget details2)
            cannotBeAttacked2 <- flip anyM modifiers2' $ \case
              CannotBeAttackedBy matcher ->
                elem (attackEnemy details2) <$> select matcher
              _ -> pure False
            if not cannotBeAttacked2
              then
                push
                  $ EnemyAttacks [EnemyAttack details, EnemyAttack details2]
              else push $ EnemyAttacks [EnemyAttack details]
          _ -> push (EnemyAttack details)
        pure g
      else pure g
  EnemyAttacks as -> do
    mNextMessage <- peekMessage
    let
      toUI msg' = case msg' of
        EnemyAttack details -> targetLabel (attackEnemy details) [msg']
        _ -> error "unhandled"
    case mNextMessage of
      Just (EnemyAttacks as2) -> do
        _ <- popMessage
        push $ EnemyAttacks $ as ++ as2
      Just aoo@(CheckAttackOfOpportunity _ _) -> do
        _ <- popMessage
        pushAll [aoo, msg]
      Just (EnemyWillAttack details2) -> do
        _ <- popMessage
        push $ EnemyAttacks (EnemyAttack details2 : as)
      _ -> do
        player <- getPlayer (gameLeadInvestigatorId g)
        push $ chooseOneAtATime player $ map toUI as
    pure g
  Flipped (AssetSource aid) card | toCardType card /= AssetType -> do
    replaceCard card.id card
    runMessage (RemoveAsset aid) g
  QuietlyRemoveFromGame target -> do
    case target of
      AssetTarget aid -> pure $ g & entitiesL . assetsL %~ deleteMap aid
      EnemyTarget aid -> pure $ g & entitiesL . enemiesL %~ deleteMap aid
      TreacheryTarget aid -> do
        popMessageMatching_ \case
          Discard _ _ (TreacheryTarget tid) -> tid == aid
          _ -> False
        pure $ g & entitiesL . treacheriesL %~ deleteMap aid
      LocationTarget aid -> pure $ g & entitiesL . locationsL %~ deleteMap aid
      _ -> error $ "Unhandled quiet removal of target: " <> show target
  RemoveFromGame (StoryTarget sid) -> do
    pure $ g & entitiesL . storiesL %~ deleteMap sid
  RemoveFromGame (AssetTarget aid) -> do
    fieldMay AssetCard aid >>= \case
      Nothing -> pure g
      Just card ->
        runMessage
          (RemoveAsset aid)
          ( g
              & removedFromPlayL
              %~ (card :)
              & entitiesL
              . assetsL
              . ix aid
              %~ overAttrs (\x -> x {assetPlacement = OutOfPlay RemovedZone})
          )
  RemoveFromGame (LocationTarget lid) -> do
    pure $ g & (entitiesL . locationsL %~ deleteMap lid)
  RemoveFromGame (ActTarget aid) -> do
    pure $ g & (entitiesL . actsL %~ deleteMap aid)
  RemoveFromGame (SkillTarget sid) -> do
    card <- field SkillCard sid
    pure
      $ g
      & (entitiesL . skillsL %~ deleteMap sid)
      & (removedFromPlayL %~ (card :))
  RemoveFromGame (EventTarget eid) -> do
    card <- field EventCard eid
    pure
      $ g
      & (entitiesL . eventsL %~ deleteMap eid)
      & (removedFromPlayL %~ (card :))
  RemovedFromGame card -> pure $ g & removedFromPlayL %~ (card :)
  PlaceEnemyOutOfPlay _oZone eid -> do
    let
      isDiscardEnemy = \case
        Discard _ _ (EnemyTarget eid') -> eid == eid'
        RemovedFromPlay (EnemySource eid') -> eid == eid'
        _ -> False
    withQueue_ $ filter (not . isDiscardEnemy)
    pure g
  EnemySpawnFromOutOfPlay _oZone miid lid eid -> do
    pushAll (resolve $ EnemySpawn miid lid eid)
    pure $ g & (activeCardL .~ Nothing) & (focusedCardsL .~ mempty)
  Discard _ _ (SearchedCardTarget cardId) -> do
    investigator' <- getActiveInvestigator
    let
      card =
        fromJustNote "must exist"
          $ find ((== cardId) . toCardId)
          $ (g ^. focusedCardsL)
          <> ( concat
                . Map.elems
                . view Investigator.foundCardsL
                $ toAttrs investigator'
             )
    case card of
      PlayerCard pc -> do
        pushAll
          [ RemoveCardFromSearch (toId investigator') cardId
          , AddToDiscard (toId investigator') pc
          ]
        pure $ g & focusedCardsL %~ filter (/= card)
      _ -> error "should not be an option for other cards"
  Discard _ _ (ActTarget aid) ->
    pure $ g & entitiesL . actsL %~ Map.filterWithKey (\k _ -> k /= aid)
  Discard _ _ (AgendaTarget aid) ->
    pure $ g & entitiesL . agendasL %~ Map.filterWithKey (\k _ -> k /= aid)
  Discarded (EnemyTarget eid) source _ -> do
    enemy <- getEnemy eid
    case attr enemyPlacement enemy of
      AsSwarm {} -> pure () -- will be handled when leaves play
      _ -> case toCard (toAttrs enemy) of
        PlayerCard pc -> case enemyBearer (toAttrs enemy) of
          Nothing -> push (RemoveFromGame $ EnemyTarget eid)
          -- The Man in the Pallid Mask has not bearer in Curtain Call
          Just iid' -> push (AddToDiscard iid' pc)
        EncounterCard _ -> pure ()
        VengeanceCard _ -> error "Vengeance card"

    miid <- getSourceController source
    mLocation <- field EnemyLocation eid

    let
      handleKey k =
        case miid of
          Nothing -> case mLocation of
            Just location -> PlaceKey (toTarget location) k
            Nothing -> error "Could not place key"
          Just iid -> PlaceKey (toTarget iid) k

    ks <- fieldMap EnemyKeys toList eid
    pushAll $ map handleKey ks

    pure g
  AddToDiscard _ pc -> pure $ g & removedFromPlayL %~ filter (/= PlayerCard pc)
  AddToVictory (EnemyTarget eid) -> do
    card <- field EnemyCard eid
    pushAll
      $ windows [Window.LeavePlay (EnemyTarget eid), Window.AddedToVictory card]
      <> [RemoveEnemy eid]
    pure
      $ g
      & entitiesL
      . enemiesL
      . ix eid
      %~ overAttrs (\x -> x {enemyPlacement = OutOfPlay RemovedZone})
  DefeatedAddToVictory (EnemyTarget eid) -> do
    -- when defeated, removal is handled by the defeat effect
    card <- field EnemyCard eid
    pushAll $ windows [Window.AddedToVictory card]
    pure g
  AddToVictory (SkillTarget sid) -> do
    card <- field SkillCard sid
    pushAll $ windows [Window.AddedToVictory card]
    pure $ g & (entitiesL . skillsL %~ deleteMap sid) -- we might not want to remove here?
  AddToVictory (EventTarget eid) -> do
    card <- field EventCard eid
    pushAll $ windows [Window.AddedToVictory card]
    pure $ g & (entitiesL . eventsL %~ deleteMap eid) -- we might not want to remove here?
  AddToVictory (ActTarget aid) -> do
    card <- field ActCard aid
    pushAll $ windows [Window.AddedToVictory card]
    pure $ g & (entitiesL . actsL %~ deleteMap aid) -- we might not want to remove here?
  AddToVictory (StoryTarget sid) -> do
    card <- field StoryCard sid
    pushAll $ windows [Window.AddedToVictory card]
    pure $ g & (entitiesL . storiesL %~ deleteMap sid)
  AddToVictory (TreacheryTarget tid) -> do
    card <- field TreacheryCard tid
    pushAll $ RemoveTreachery tid : windows [Window.AddedToVictory card]
    pure g
  AddToVictory (LocationTarget lid) -> do
    card <- field LocationCard lid
    pushAll $ RemoveLocation lid : windows [Window.AddedToVictory card]
    pure g
  PlayerWindow iid _ _ -> pure $ g & activeInvestigatorIdL .~ iid
  Begin InvestigationPhase -> do
    let phaseStep step msgs = Msg.PhaseStep (InvestigationPhaseStep step) msgs
    investigatorIds <- getInvestigators
    phaseBeginsWindow <-
      checkWindows
        [ mkWhen Window.AnyPhaseBegins
        , mkWhen (Window.PhaseBegins #investigation)
        , mkAfter Window.AnyPhaseBegins
        , mkAfter (Window.PhaseBegins #investigation)
        ]

    fastWindow <- checkWindows [mkWhen Window.FastPlayerWindow]
    case investigatorIds of
      [] -> error "no investigators"
      [iid] ->
        pushAll
          [ phaseStep InvestigationPhaseBeginsStep [phaseBeginsWindow]
          , phaseStep InvestigationPhaseBeginsWindow [fastWindow]
          , phaseStep NextInvestigatorsTurnBeginsStep [ChoosePlayer iid SetTurnPlayer]
          ]
      xs -> do
        player <- getPlayer (g ^. leadInvestigatorIdL)
        pushAll
          [ phaseStep InvestigationPhaseBeginsStep [phaseBeginsWindow]
          , phaseStep InvestigationPhaseBeginsWindow [fastWindow]
          , phaseStep
              NextInvestigatorsTurnBeginsStep
              [ questionLabel "Choose player to take turn" player
                  $ ChooseOne
                    [PortraitLabel iid [ChoosePlayer iid SetTurnPlayer] | iid <- xs]
              ]
          ]
    pure $ g & phaseL .~ InvestigationPhase
  BeginTurn x -> do
    player <- getPlayer x
    pushM $ checkWindows [mkWhen (Window.TurnBegins x), mkAfter (Window.TurnBegins x)]
    pure
      $ g
      & (activeInvestigatorIdL .~ x)
      & (activePlayerIdL .~ player)
      & (turnPlayerInvestigatorIdL ?~ x)
      & (activeAbilitiesL .~ mempty)
      & (actionRemovedEntitiesL .~ mempty)
      & (entitiesL %~ clearRemovedEntities)
  ChoosePlayerOrder _ [x] [] -> do
    pure $ g & playerOrderL .~ [x]
  ChoosePlayerOrder _ [] (x : xs) -> do
    pure $ g & playerOrderL .~ (x : xs)
  ChoosePlayerOrder _ [y] (x : xs) -> do
    pure $ g & playerOrderL .~ (x : (xs <> [y]))
  ChoosePlayerOrder lead investigatorIds orderedInvestigatorIds -> do
    player <- getPlayer lead
    push
      $ questionLabel "Choose next in turn order" player
      $ ChooseOne
        [ PortraitLabel
          iid
          [ ChoosePlayerOrder
              iid
              (filter (/= iid) investigatorIds)
              (orderedInvestigatorIds <> [iid])
          ]
        | iid <- investigatorIds
        ]
    pure $ g & activeInvestigatorIdL .~ gameLeadInvestigatorId g
  ChooseEndTurn iid -> do
    wouldWindow <- checkWindows [mkWhen $ Window.WouldEndTurn iid]
    msgs <- resolveWithWindow (EndTurn iid) (Window.TurnEnds iid)
    pushAll $ wouldWindow : msgs
    pure g
  After (EndTurn _) ->
    pure $ g & turnHistoryL .~ mempty & turnPlayerInvestigatorIdL .~ Nothing
  After EndPhase -> do
    clearQueue
    case g ^. phaseL of
      MythosPhase {} -> pushEnd $ Begin InvestigationPhase
      InvestigationPhase {} -> pushEnd $ Begin EnemyPhase
      EnemyPhase {} -> pushEnd $ Begin UpkeepPhase
      UpkeepPhase {} -> pushAllEnd [EndRoundWindow, EndRound]
      ResolutionPhase {} -> error "should not be called in this situation"
      CampaignPhase {} -> error "should not be called in this situation"
    pure
      $ g
      & (roundHistoryL %~ (<> view phaseHistoryL g))
      & (phaseHistoryL %~ mempty)
  EndInvestigation -> do
    whenWindow <- checkWindows [mkWhen (Window.PhaseEnds InvestigationPhase)]
    afterWindow <- checkWindows [mkAfter (Window.PhaseEnds InvestigationPhase)]

    pushAll [whenWindow, EndPhase, afterWindow, After EndPhase]
    pure
      $ g
      & (phaseHistoryL .~ mempty)
      & (roundHistoryL %~ (<> view phaseHistoryL g))
      & (turnPlayerInvestigatorIdL .~ Nothing)
  Begin EnemyPhase -> do
    phaseBeginsWindow <-
      checkWindows
        [ mkWhen Window.AnyPhaseBegins
        , mkWhen (Window.PhaseBegins EnemyPhase)
        , mkAfter Window.AnyPhaseBegins
        , mkAfter (Window.PhaseBegins EnemyPhase)
        ]
    enemiesAttackWindow <-
      checkWindows
        [mkWhen Window.EnemiesAttackStep]
    afterHuntersMoveWindow <-
      checkWindows
        [mkAfter Window.HuntersMoveStep]

    fastWindow <- checkWindows [mkWhen Window.FastPlayerWindow]
    let phaseStep step msgs = Msg.PhaseStep (EnemyPhaseStep step) msgs
    pushAllEnd
      [ phaseStep EnemyPhaseBeginsStep [phaseBeginsWindow]
      , phaseStep HunterEnemiesMoveStep [HuntersMove, afterHuntersMoveWindow]
      , phaseStep ResolveAttacksWindow [fastWindow, enemiesAttackWindow]
      , phaseStep ResolveAttacksStep [EnemiesAttack]
      , phaseStep EnemyPhaseEndsStep [EndEnemy]
      ]
    pure $ g & phaseL .~ EnemyPhase
  EnemyAttackFromDiscard iid source card -> do
    enemyId <- getRandom
    let enemy = overAttrs (\a -> a {enemyPlacement = StillInEncounterDiscard}) (createEnemy card enemyId)
    pushAll
      [ EnemyWillAttack
          $ (enemyAttack enemyId source iid)
            { attackDamageStrategy = enemyDamageStrategy (toAttrs enemy)
            }
      , RemoveEnemy enemyId
      ]

    pure $ g & entitiesL . enemiesL . at enemyId ?~ enemy
  EndEnemy -> do
    pushAll
      . (: [EndPhase, After EndPhase])
      =<< checkWindows
        [mkWhen (Window.PhaseEnds EnemyPhase)]
    pure
      $ g
      & (roundHistoryL %~ (<> view phaseHistoryL g))
      & (phaseHistoryL .~ mempty)
  Begin UpkeepPhase -> do
    let phaseStep step msgs = Msg.PhaseStep (UpkeepPhaseStep step) msgs
    phaseBeginsWindow <-
      checkWindows
        [ mkWhen Window.AnyPhaseBegins
        , mkWhen (Window.PhaseBegins UpkeepPhase)
        , mkAfter Window.AnyPhaseBegins
        , mkAfter (Window.PhaseBegins UpkeepPhase)
        ]
    fastWindow <- checkWindows [mkWhen Window.FastPlayerWindow]
    pushAllEnd
      [ phaseStep UpkeepPhaseBeginsStep [phaseBeginsWindow]
      , phaseStep UpkeepPhaseBeginsWindow [fastWindow]
      , phaseStep ResetActionsStep []
      , phaseStep ReadyExhaustedStep [ReadyExhausted]
      , phaseStep DrawCardAndGainResourceStep [AllDrawCardAndResource]
      , phaseStep CheckHandSizeStep [AllCheckHandSize]
      , phaseStep UpkeepPhaseEndsStep [EndUpkeep, Do EndUpkeep]
      ]
    pure $ g & phaseL .~ UpkeepPhase
  Do EndUpkeep -> do
    pushAll
      . (: [EndPhase, After EndPhase])
      =<< checkWindows
        [mkWhen (Window.PhaseEnds UpkeepPhase)]
    pure
      $ g
      & (roundHistoryL %~ (<> view phaseHistoryL g))
      & (phaseHistoryL .~ mempty)
  EndRoundWindow -> do
    windows' <-
      traverse
        (\t -> checkWindows [mkWindow t Window.AtEndOfRound])
        [#when, Timing.AtIf, #after]
    pushAll windows'
    pure g
  BeginRoundWindow -> do
    windows' <-
      traverse
        (\t -> checkWindows [mkWindow t Window.AtBeginningOfRound])
        [#when, Timing.AtIf, #after]
    pushAll windows'
    pure g
  EndRound -> do
    pushAllEnd [BeginRoundWindow, BeginRound, Begin MythosPhase]
    let
      isPerRound = \case
        MaxPerRound _ -> True
        MaxPerTraitPerRound _ _ -> True
        _ -> False
    let roundEndUses =
          map cdCardCode
            . filter (any isPerRound . cdLimits)
            . mapMaybe lookupCardDef
            $ Map.keys (view cardUsesL g)
    pure
      $ g
      & (roundHistoryL .~ mempty)
      & cardUsesL
      %~ Map.filterWithKey (\k _ -> k `notElem` roundEndUses)
  Begin MythosPhase {} -> do
    let playerOrder = g ^. playerOrderL
    mGloria <- selectOne $ investigatorIs Investigators.gloriaGoldberg
    hasEncounterDeck <- scenarioField ScenarioHasEncounterDeck
    phaseBeginsWindow <-
      checkWindows
        [ mkWhen Window.AnyPhaseBegins
        , mkWhen (Window.PhaseBegins MythosPhase)
        , mkAfter Window.AnyPhaseBegins
        , mkAfter (Window.PhaseBegins MythosPhase)
        ]
    allDrawWindow <-
      checkWindows
        [mkWhen Window.AllDrawEncounterCard]
    afterCheckDoomThreshold <-
      checkWindows
        [mkWhen Window.AfterCheckDoomThreshold]
    fastWindow <- checkWindows [mkWhen Window.FastPlayerWindow]
    modifiers <- getModifiers (PhaseTarget MythosPhase)
    let phaseStep s msgs = Msg.PhaseStep (MythosPhaseStep s) msgs
    pushAllEnd
      $ phaseStep
        MythosPhaseBeginsStep
        (phaseBeginsWindow : [ChoosePlayerOrder gloria playerOrder [] | gloria <- toList mGloria])
      : [ phaseStep PlaceDoomOnAgendaStep [placeDoomOnAgenda]
        | SkipMythosPhaseStep PlaceDoomOnAgendaStep `notElem` modifiers
        ]
        <> [phaseStep CheckDoomThresholdStep [AdvanceAgendaIfThresholdSatisfied, afterCheckDoomThreshold]]
        <> [ phaseStep EachInvestigatorDrawsEncounterCardStep [allDrawWindow, AllDrawEncounterCard]
           | hasEncounterDeck
           ]
        <> [ phaseStep MythosPhaseWindow [fastWindow]
           , phaseStep
              MythosPhaseEndsStep
              [EndMythos, ChoosePlayerOrder (gameLeadInvestigatorId g) [] playerOrder]
           ]
    pure $ g & phaseL .~ MythosPhase & phaseStepL ?~ MythosPhaseStep MythosPhaseBeginsStep
  Msg.PhaseStep step msgs -> do
    pushAll msgs
    pure $ g & phaseStepL ?~ step
  AllDrawEncounterCard -> do
    investigators <- filterM (fmap not . isEliminated) =<< getInvestigatorsInOrder
    push $ SetActiveInvestigator $ g ^. activeInvestigatorIdL
    for_ (reverse investigators) \iid -> push $ ForInvestigator iid AllDrawEncounterCard
    pure g
  ForInvestigator iid AllDrawEncounterCard -> do
    whenM (not <$> isEliminated iid) do
      player <- getPlayer iid
      push $ chooseOne player [TargetLabel EncounterDeckTarget [drawEncounterCard iid GameSource]]
    pure $ g & activeInvestigatorIdL .~ iid
  EndMythos -> do
    pushAll
      . (: [EndPhase, After EndPhase])
      =<< checkWindows [mkWhen (Window.PhaseEnds MythosPhase)]
    pure
      $ g
      & (roundHistoryL %~ (<> view phaseHistoryL g))
      & (phaseHistoryL .~ mempty)
  BeginSkillTestWithPreMessages _ pre skillTest -> runMessage (BeginSkillTestWithPreMessages' pre skillTest) g
  BeginSkillTestWithPreMessages' pre skillTest -> do
    runQueueT $ handleSkillTestNesting skillTest.id msg g do
      let iid = skillTest.investigator
      let windows' = windows [Window.InitiatedSkillTest skillTest]
      let defaultCase = windows' <> [BeginSkillTestAfterFast]

      performRevelationSkillTestWindow <-
        checkWindows [mkWhen $ Window.WouldPerformRevelationSkillTest iid skillTest.id]

      msgs <- case skillTestType skillTest of
        ResourceSkillTest -> pure defaultCase
        BaseValueSkillTest {} -> pure defaultCase
        SkillSkillTest skillType -> do
          availableSkills <- getAvailableSkillsFor skillType iid
          player <- getPlayer iid
          pure
            $ if Set.size availableSkills < 2
              then defaultCase
              else
                [ chooseOne
                    player
                    $ SkillLabel skillType []
                    : [ SkillLabel skillType' [ReplaceSkillTestSkill (FromSkillType skillType) (ToSkillType skillType')]
                      | skillType' <- setToList availableSkills
                      , skillType' /= skillType
                      ]
                ]
                  <> windows'
                  <> [BeginSkillTestAfterFast]
        AndSkillTest types -> do
          availableSkills <- for types $ traverseToSnd (`getAvailableSkillsFor` iid)
          -- (base, other choices)
          let skillsWithChoice = filter ((> 1) . Set.size . snd) availableSkills
          if null skillsWithChoice
            then pure defaultCase
            else do
              -- if we have base skills with other choices we need to choose for each one
              -- if we choose a type it should replace for example if we have int+agi+wil+com and we use mind over matter
              -- we should be asked for agi and com and end up with int+int+wil+int
              -- Easiest way might be to let the skill test handle the replacement so we don't have to nest
              player <- getPlayer skillTest.investigator
              pure
                $ map
                  ( \(base, setToList -> skillsTypes) ->
                      chooseOne player
                        $ SkillLabel base []
                        : [ SkillLabel skillType' [ReplaceSkillTestSkill (FromSkillType base) (ToSkillType skillType')]
                          | skillType' <- skillsTypes
                          , skillType' /= base
                          ]
                  )
                  skillsWithChoice
                <> windows'
                <> [BeginSkillTestAfterFast]

      msgs' <-
        if skillTestIsRevelation skillTest
          then pure $ performRevelationSkillTestWindow : msgs
          else pure msgs

      pushAll
        $ [SetActiveInvestigator skillTest.investigator]
        <> pre
        <> msgs'
        <> [SetActiveInvestigator (g ^. activeInvestigatorIdL)]
      pure $ g & (skillTestL ?~ skillTest)
  BeforeSkillTest skillTestId -> do
    getSkillTest >>= \case
      Nothing -> pure g
      Just skillTest ->
        if skillTestId == skillTest.id
          then do
            player <- getPlayer skillTest.investigator
            pure $ g & activeInvestigatorIdL .~ skillTest.investigator & activePlayerIdL .~ player
          else pure g
  CreateStoryAssetAtLocationMatching cardCode locationMatcher -> do
    lid <- selectJust locationMatcher
    assetId <- getRandom
    push $ CreateAssetAt assetId cardCode $ AtLocation lid
    pure g
  ReadStory iid card storyMode mtarget -> do
    placement <- case mtarget of
      Just (EnemyTarget eid) -> field EnemyPlacement eid
      Just (AssetTarget aid) -> field AssetPlacement aid
      Just _ -> error "no known placement for non-enemy target"
      Nothing -> pure Unplaced
    push $ ReadStoryWithPlacement iid card storyMode mtarget placement
    pure g
  ReadStoryWithPlacement iid card storyMode mtarget placement -> do
    let
      storyId = StoryId $ toCardCode card
      story' = overAttrs (Story.placementL .~ placement) (createStory card mtarget storyId)
    -- if we have a target the ui should visually replace them, otherwise we add to UI by focus
    player <- getPlayer iid
    case storyPlacement (toAttrs story') of
      Unplaced ->
        pushAll
          [ FocusCards [card]
          , chooseOne
              player
              [targetLabel (toCardId card) [ResolveStory iid storyMode storyId, ResolvedStory storyMode storyId]]
          ]
      _ ->
        push
          $ chooseOne
            player
            [ targetLabel (toTarget storyId) [ResolveStory iid storyMode storyId, ResolvedStory storyMode storyId]
            ]
    pure $ g & entitiesL . storiesL . at storyId ?~ story'
  PlaceStory card placement -> do
    let storyId = StoryId $ toCardCode card
    let story' = overAttrs (Story.placementL .~ placement) (createStory card Nothing storyId)
    pure $ g & entitiesL . storiesL . at storyId ?~ story'
  ResolveStory _ _ sid -> do
    card <- field StoryCard sid
    pure $ g & focusedCardsL %~ filter (/= card)
  RemoveStory storyId -> do
    pure $ g & entitiesL . storiesL %~ deleteMap storyId
  CreateSkill skillId card investigatorId placement -> do
    let skill = createSkill card investigatorId skillId
    pure
      $ g
      & entitiesL
      . skillsL
      . at skillId
      ?~ overAttrs (\a -> a {skillPlacement = placement}) skill
  PutCardIntoPlayWithAdditionalCosts iid card mTarget payment ws -> do
    createActiveCostForAdditionalCardCosts iid card >>= \case
      Nothing -> do
        push $ PutCardIntoPlay iid card mTarget payment ws
        pure g
      Just cost -> do
        pushAll [PutCardIntoPlay iid card mTarget payment ws, CreatedCost (activeCostId cost)]
        pure $ g & (activeCostL %~ insertMap (activeCostId cost) cost)
  CreateAssetAt assetId card placement -> do
    let asset = createAsset card assetId
    iid <- getActiveInvestigatorId
    mCost <- createActiveCostForAdditionalCardCosts iid card
    case mCost of
      Nothing -> do
        push $ PlaceAsset assetId placement
        pure $ g & entitiesL . assetsL . at assetId ?~ asset
      Just cost -> do
        pushAll [CreatedCost (activeCostId cost), PlaceAsset assetId placement]
        pure
          $ g
          & (entitiesL . assetsL . at assetId ?~ asset)
          & (activeCostL %~ insertMap (activeCostId cost) cost)
  CreateEventAt iid card placement -> do
    eventId <- getRandom
    let event' = createEvent card iid eventId
    mCost <- createActiveCostForAdditionalCardCosts iid card
    case mCost of
      Nothing -> do
        push $ PlaceEvent eventId placement
        pure $ g & entitiesL . eventsL . at eventId ?~ event'
      Just cost -> do
        pushAll
          [CreatedCost (activeCostId cost), PlaceEvent eventId placement]
        pure
          $ g
          & (entitiesL . eventsL . at eventId ?~ event')
          & (activeCostL %~ insertMap (activeCostId cost) cost)
  CreateWeaknessInThreatArea card iid -> do
    treacheryId <- getRandom
    let treachery = createTreachery card iid treacheryId
    push (PlaceTreachery treacheryId (InThreatArea iid))
    pure $ g & entitiesL . treacheriesL . at treacheryId ?~ treachery
  AttachStoryTreacheryTo treacheryId card target -> do
    let treachery = createTreachery card (g ^. leadInvestigatorIdL) treacheryId
    push
      $ PlaceTreachery treacheryId
      $ case target of
        LocationTarget lid -> AttachedToLocation lid
        EnemyTarget eid -> AttachedToEnemy eid
        AssetTarget aid -> AttachedToAsset aid Nothing
        ActTarget aid -> AttachedToAct aid
        AgendaTarget aid -> AttachedToAgenda aid
        InvestigatorTarget iid -> InThreatArea iid
        AgendaDeckTarget -> NextToAgenda
        _ -> error $ "unhandled attach target : " <> show target
    pure $ g & entitiesL . treacheriesL . at treacheryId ?~ treachery
  TakeControlOfSetAsideAsset iid card -> do
    assetId <- getRandom
    let asset = createAsset card assetId
    pushAll [TakeControlOfAsset iid assetId]
    pure $ g & entitiesL . assetsL . at assetId ?~ asset
  ReplaceInvestigatorAsset iid assetId card -> do
    tokens <- field AssetTokens assetId
    underneath <- field AssetCardsUnderneath assetId
    replaceCard (toCardId card) card
    let asset =
          overAttrs
            (\attrs -> attrs {assetTokens = tokens, assetCardsUnderneath = underneath})
            (createAsset card assetId)
    push (ReplacedInvestigatorAsset iid assetId)
    pure $ g & entitiesL . assetsL . at assetId ?~ asset
  ReplaceAsset assetId cardDef -> do
    asset <- getAsset assetId
    let card = lookupCard cardDef (toCardId asset)
    replaceCard (toCardId card) card
    let asset' =
          overAttrs
            ( \attrs ->
                attrs
                  { assetTokens = attr assetTokens asset
                  , assetCardsUnderneath = attr assetCardsUnderneath asset
                  , assetPlacement = attr assetPlacement asset
                  , assetOwner = attr assetOwner asset
                  , assetController = attr assetController asset
                  , assetExhausted = attr assetExhausted asset
                  , assetSealedChaosTokens = attr assetSealedChaosTokens asset
                  , assetKeys = attr assetKeys asset
                  , assetDriver = attr assetDriver asset
                  }
            )
            (createAsset card assetId)
    pure $ g & entitiesL . assetsL . at assetId ?~ asset'
  When (EnemySpawn _ lid eid) -> do
    windowMsg <- checkWindows [mkWhen (Window.EnemySpawns eid lid)]
    g <$ push windowMsg
  After (EnemySpawn _ lid eid) -> do
    windowMsg <- checkWindows [mkAfter (Window.EnemySpawns eid lid)]
    g <$ push windowMsg
  -- TODO: CHECK SpawnEnemyAt and SpawnEnemyAtEngagedWith
  SpawnEnemyAt card lid -> do
    enemyId <- getRandom
    let enemy = createEnemy card enemyId
    pushAll
      [ Will (EnemySpawn Nothing lid enemyId)
      , When (EnemySpawn Nothing lid enemyId)
      , EnemySpawn Nothing lid enemyId
      , After (EnemySpawn Nothing lid enemyId)
      ]
    pure $ g & entitiesL . enemiesL . at enemyId ?~ enemy
  SpawnEnemyAtEngagedWith card lid iid -> do
    enemyId <- getRandom
    let enemy = createEnemy card enemyId
    pushAll
      [ Will (EnemySpawn (Just iid) lid enemyId)
      , When (EnemySpawn (Just iid) lid enemyId)
      , EnemySpawn (Just iid) lid enemyId
      ]
    pure $ g & entitiesL . enemiesL . at enemyId ?~ enemy
  CreateEnemy enemyCreation -> do
    let enemyId = enemyCreationEnemyId enemyCreation
        card = enemyCreationCard enemyCreation
        mTarget = enemyCreationTarget enemyCreation
        originalCardCode = \case
          EncounterCard ec -> ecOriginalCardCode ec
          PlayerCard pc -> pcOriginalCardCode pc
          VengeanceCard vc -> originalCardCode vc
        getBearer = \case
          EncounterCard _ -> Nothing
          PlayerCard pc -> pcOwner pc
          VengeanceCard vc -> getBearer vc
    enemy'' <-
      runMessage
        (SetOriginalCardCode $ originalCardCode card)
        (createEnemy card enemyId)

    let
      miid = enemyCreationInvestigator enemyCreation
      enemy' =
        overAttrs (\attrs -> attrs {enemySpawnedBy = miid})
          $ if enemyCreationExhausted enemyCreation
            then overAttrs (\attrs -> attrs {enemyExhausted = True}) enemy''
            else enemy''

    enemy <- case getBearer card of
      Nothing -> pure enemy'
      Just iid -> runMessage (SetBearer (toTarget enemy') iid) enemy'
    case enemyCreationMethod enemyCreation of
      SpawnEngagedWith iid -> do
        lid <- getJustLocation iid
        pushAll
          $ enemyCreationBefore enemyCreation
          <> [ Will (EnemySpawn (Just iid) lid enemyId)
             , When (EnemySpawn (Just iid) lid enemyId)
             , EnemySpawn (Just iid) lid enemyId
             ]
          <> [CreatedEnemyAt enemyId lid target | target <- maybeToList mTarget]
          <> enemyCreationAfter enemyCreation
          <> [After (EnemySpawn (Just iid) lid enemyId)]
      SpawnAtLocation lid -> do
        windows' <- checkWindows [mkWhen (Window.EnemyWouldSpawnAt enemyId lid)]
        pushAll
          $ windows'
          : enemyCreationBefore enemyCreation
            <> [ Will (EnemySpawn Nothing lid enemyId)
               , When (EnemySpawn Nothing lid enemyId)
               , EnemySpawn Nothing lid enemyId
               ]
            <> [CreatedEnemyAt enemyId lid target | target <- maybeToList mTarget]
            <> enemyCreationAfter enemyCreation
            <> [After (EnemySpawn Nothing lid enemyId)]
      SpawnAtLocationMatching locationMatcher -> do
        matches' <- select locationMatcher
        case matches' of
          [] -> push (toDiscard GameSource (toTarget enemyId))
          lids -> do
            lead <- getLead
            player <- getPlayer $ fromMaybe lead miid
            pushAll
              $ windows [Window.EnemyAttemptsToSpawnAt enemyId locationMatcher]
              <> [ chooseOrRunOne
                    player
                    [ targetLabel lid [CreateEnemy $ enemyCreation {enemyCreationMethod = SpawnAtLocation lid}]
                    | lid <- lids
                    ]
                 ]
      SpawnWithPlacement placement -> do
        mLocation <- getPlacementLocation placement
        let
          (beforeMessages, afterMessages) = case mLocation of
            Nothing -> ([], [])
            Just lid ->
              ( [Will (EnemySpawn Nothing lid enemyId), When (EnemySpawn Nothing lid enemyId)]
              , [After (EnemySpawn Nothing lid enemyId)]
              )
        pushAll
          $ enemyCreationBefore enemyCreation
          <> beforeMessages
          <> [PlaceEnemy enemyId placement]
          <> enemyCreationAfter enemyCreation
          <> afterMessages
      SpawnEngagedWithPrey ->
        pushAll
          $ enemyCreationBefore enemyCreation
          <> [ Will (EnemySpawnEngagedWithPrey enemyId)
             , EnemySpawnEngagedWithPrey enemyId
             ]
          <> enemyCreationAfter enemyCreation
      SpawnViaSpawnInstruction -> spawnAt enemyId miid (fromMaybe (error "called without spawn at") $ attr enemySpawnAt enemy)
    pure $ g & entitiesL . enemiesL . at enemyId ?~ enemy
  Discarded (InvestigatorTarget iid) source card -> do
    pushM
      $ checkWindows
      $ (`mkWindow` Window.Discarded (Just iid) source card)
      <$> [#when, #after]
    pure g
  InvestigatorAssignDamage iid' source _ n 0 | n > 0 -> do
    miid <- getSourceController source
    case miid of
      Nothing -> pure g
      Just iid -> do
        let
          historyItem = HistoryItem HistoryDealtDamageTo [InvestigatorTarget iid']
          turn = isJust $ view turnPlayerInvestigatorIdL g
          setTurnHistory = if turn then turnHistoryL %~ insertHistory iid historyItem else id

        pure $ g & (phaseHistoryL %~ insertHistory iid historyItem) & setTurnHistory
  Msg.EnemyDamage eid assignment@(damageAssignmentAmount -> n) | n > 0 -> do
    let source = damageAssignmentSource assignment
    miid <- getSourceController source
    lead <- getLead
    -- TODO: This is wrong but history is the way we track if enemies were
    -- defeated for cards like Kerosene (1), we need a history independent of
    -- the iid for cases where we aren't looking at a specific investigator
    let
      iid = fromMaybe lead miid
      historyItem = HistoryItem HistoryDealtDamageTo [EnemyTarget eid]
      turn = isJust $ view turnPlayerInvestigatorIdL g
      setTurnHistory =
        if turn then turnHistoryL %~ insertHistory iid historyItem else id

    pure $ g & (phaseHistoryL %~ insertHistory iid historyItem) & setTurnHistory
  FoundEncounterCardFrom {} -> pure $ g & (focusedCardsL .~ mempty)
  FoundAndDrewEncounterCard {} -> pure $ g & (focusedCardsL .~ mempty)
  SearchCollectionForRandom iid source matcher -> do
    investigatorClass <- field Investigator.InvestigatorClass iid
    playerCount <- getPlayerCount
    let
      multiplayerFilter =
        if playerCount < 2
          then notElem MultiplayerOnly . cdDeckRestrictions . toCardDef
          else const True
      notForClass = \case
        OnlyClass c -> c /= investigatorClass
        _ -> True
      classOnlyFilter = not . any notForClass . cdDeckRestrictions . toCardDef
      cardFilter = and . sequence [multiplayerFilter, classOnlyFilter, (`cardMatch` matcher)]
    mcard <-
      case filter
        (cardFilter . (`lookupPlayerCard` nullCardId))
        (toList allPlayerCards) of
        [] -> pure Nothing
        (x : xs) -> Just <$> (genPlayerCard =<< sample (x :| xs))
    g <$ push (RequestedPlayerCard iid source mcard [])
  CancelSurge _ -> do
    ems <- effectModifiers GameSource [NoSurge]
    for_ (view resolvingCardL g) $ \c -> do
      push
        $ CreateWindowModifierEffect (EffectCardResolutionWindow c.id) ems GameSource (CardIdTarget c.id)
    pure g
  GainSurge source target -> do
    cardId <- case target of
      EnemyTarget eid -> field EnemyCardId eid
      TreacheryTarget tid -> field TreacheryCardId tid
      AssetTarget aid -> field AssetCardId aid
      LocationTarget lid -> field LocationCardId lid
      CardIdTarget cid -> pure cid
      _ -> error "Unhandled surge target"
    (effectId, surgeEffect) <- createSurgeEffect source cardId
    pure $ g & entitiesL . effectsL . at effectId ?~ surgeEffect
  Surge iid source -> g <$ push (drawEncounterCard iid source)
  ReplaceCard cardId card -> do
    replaceCard cardId card -- We must update the IORef
    pure $ g & cardsL %~ insertMap cardId card
  After (InvestigatorEliminated iid) -> pure $ g & playerOrderL %~ filter (/= iid)
  SetActivePlayer pid -> do
    let investigator =
          fromJustNote "No such player"
            $ find (\i -> i.player == pid) (toList $ g ^. entitiesL . investigatorsL)
    pure $ g & activeInvestigatorIdL .~ investigator.id & activePlayerIdL .~ pid
  SetActiveInvestigator iid -> do
    player <- getPlayer iid
    pure $ g & activeInvestigatorIdL .~ iid & activePlayerIdL .~ player
  -- InvestigatorDrawEncounterCard iid -> do
  --   drawEncounterCardWindow <- checkWindows [mkWhen $ Window.WouldDrawEncounterCard iid $ g ^. phaseL]
  --   pushAll
  --     [ SetActiveInvestigator iid
  --     , drawEncounterCardWindow
  --     , InvestigatorDoDrawEncounterCard iid
  --     , SetActiveInvestigator (g ^. activeInvestigatorIdL)
  --     ]
  --   pure g
  RevelationSkillTest sid iid (TreacherySource tid) skillType difficulty -> do
    -- [ALERT] If changed update (DreamersCurse, Somniphobia)
    card <- field TreacheryCard tid

    let
      skillTest =
        (initSkillTest sid iid tid tid skillType difficulty)
          { skillTestIsRevelation = True
          }
    pushAll [BeginSkillTest skillTest, UnsetActiveCard]
    pure $ g & (activeCardL ?~ card)
  Revelation iid (CardIdSource cid) -> do
    card <- getCard cid
    case toCardType card of
      AssetType -> do
        pid <- getPlayer iid
        sendRevelation pid (toJSON $ toCard card)
        assetId <- getRandom
        let asset = createAsset card assetId
        -- Asset is assumed to have a revelation ability if drawn from encounter deck
        pushAll $ resolve $ Revelation iid (AssetSource assetId)
        pure $ g & (entitiesL . assetsL . at assetId ?~ asset)
      EventType -> do
        pid <- getPlayer iid
        sendRevelation pid (toJSON $ toCard card)
        eventId <- getRandom
        pushAll $ resolve $ Revelation iid (EventSource eventId)
        let
          recordLimit g'' = \case
            MaxPerGame _ -> g'' & cardUsesL . at (toCardCode card) . non 0 +~ 1
            MaxPerRound _ -> g'' & cardUsesL . at (toCardCode card) . non 0 +~ 1
            MaxPerTraitPerRound _ _ -> g'' & cardUsesL . at (toCardCode card) . non 0 +~ 1
            _ -> g''
        pure
          $ foldl'
            recordLimit
            (g & entitiesL . eventsL . at eventId ?~ createEvent card iid eventId)
            (cdLimits $ toCardDef card)
      PlayerEnemyType -> do
        enemyId <- getRandom
        let enemy = createEnemy card enemyId
        -- Asset is assumed to have a revelation ability if drawn from encounter deck
        pushAll
          $ [ SetBearer (toTarget enemy) iid
            , RemoveCardFromHand iid (toCardId card)
            , InvestigatorDrawEnemy iid enemyId
            ]
          <> resolve (Revelation iid (EnemySource enemyId))
        pure $ g & (entitiesL . enemiesL . at enemyId ?~ enemy)
      other ->
        error $ "Currently not handling Revelations from type " <> show other
  ResolvedCard iid card | Just card == gameResolvingCard g -> do
    modifiers' <- getModifiers (toCardId card)
    push $ After msg
    when
      ( NoSurge
          `notElem` modifiers'
          && (AddKeyword Keyword.Surge `elem` modifiers' || Keyword.Surge `elem` cdKeywords (toCardDef card))
      )
      $ push
      $ Surge iid GameSource
    let
      unsetActiveCard = \case
        Just c | c == card -> Nothing
        other -> other
    pure $ g & resolvingCardL .~ Nothing & activeCardL %~ unsetActiveCard
  InvestigatorDrewEncounterCard iid card -> do
    hasForesight <- hasModifier iid (Foresight $ toTitle card)
    whenDraw <- checkWindows [mkWhen (Window.DrawCard iid (toCard card) Deck.EncounterDeck)]
    let uiRevelation = getPlayer iid >>= (`sendRevelation` (toJSON $ toCard card))
    case toCardType card of
      EnemyType -> do
        investigator <- getInvestigator iid
        sendEnemy (toTitle investigator <> " drew Enemy") (toJSON $ toCard card)
      TreacheryType -> uiRevelation
      EncounterAssetType -> uiRevelation
      EncounterEventType -> uiRevelation
      LocationType -> uiRevelation
      _ -> pure ()
    if hasForesight
      then do
        canCancel <- EncounterCard card <=~> CanCancelRevelationEffect #any
        if canCancel
          then do
            player <- getPlayer iid
            push
              $ chooseOne
                player
                [ Label
                    "Cancel card effects and discard it"
                    [UnfocusCards, CancelNext GameSource RevelationMessage, AddToEncounterDiscard card]
                , Label "Draw as normal" [UnfocusCards, whenDraw, Do msg]
                ]
            pure $ g & focusedCardsL .~ [toCard card]
          else do
            pushAll [FocusCards [toCard card], whenDraw, Do msg]
            pure g
      else do
        pushAll [FocusCards [toCard card], whenDraw, Do msg]
        pure g
  Do (InvestigatorDrewEncounterCard iid card) -> do
    push $ ResolvedCard iid (toCard card)
    let
      removeCard = filter ((/= Just card) . preview _EncounterCard)
      g' =
        g
          & (resolvingCardL ?~ toCard card)
          & (focusedCardsL %~ removeCard)
          & (foundCardsL %~ Map.map removeCard)

    afterDraw <- checkWindows [mkAfter (Window.DrawCard iid (toCard card) Deck.EncounterDeck)]
    -- [ALERT]: If you extend this make sure to update LetMeHandleThis
    --
    modifiers' <- getModifiers card
    let ignoreRevelation = IgnoreRevelation `elem` modifiers'
    case toCardType card of
      StoryType -> do
        pushAll [afterDraw, ReadStory iid (toCard card) ResolveIt Nothing, UnsetActiveCard]
        pure g'
      EnemyType -> do
        enemyId <- getRandom
        let enemy = createEnemy card enemyId
        pushAll
          $ [afterDraw, InvestigatorDrawEnemy iid enemyId]
          <> [Revelation iid (EnemySource enemyId) | hasRevelation card && not ignoreRevelation]
          <> [UnsetActiveCard]
        pure
          $ g'
          & (entitiesL . enemiesL . at enemyId ?~ enemy)
          & (activeCardL ?~ toCard card)
      TreacheryType -> do
        -- handles draw windows
        pushAll [DrewTreachery iid (Just Deck.EncounterDeck) (toCard card)]
        pure g'
      EncounterAssetType -> do
        assetId <- getRandom
        let asset = createAsset card assetId
        -- Asset is assumed to have a revelation ability if drawn from encounter deck
        pushAll $ afterDraw
          : (guard (not ignoreRevelation) *> resolve (Revelation iid $ AssetSource assetId))
        pure $ g' & (entitiesL . assetsL . at assetId ?~ asset)
      EncounterEventType -> do
        eventId <- getRandom
        let owner = fromMaybe iid (toCardOwner card)
        let event' = createEvent card owner eventId
        -- Event is assumed to have a revelation ability if drawn from encounter deck
        pushAll $ afterDraw
          : (guard (not ignoreRevelation) *> resolve (Revelation iid $ EventSource eventId))
        pure $ g' & (entitiesL . eventsL . at eventId ?~ event')
      LocationType -> do
        locationId <- getRandom
        let location = createLocation card locationId

        pushAll
          $ afterDraw
          : PlacedLocation (toName location) (toCardCode card) locationId
          : (guard (not ignoreRevelation) *> resolve (Revelation iid (LocationSource locationId)))
        pure $ g' & (entitiesL . locationsL . at locationId ?~ location)
      _ ->
        error
          $ "Unhandled card type: "
          <> show (toCardType card)
          <> ": "
          <> show card
  ResolveRevelation iid card -> do
    getPlayer iid >>= (`sendRevelation` (toJSON $ toCard card))
    let
      removeCard = filter (/= card)
      g' =
        g
          & (resolvingCardL ?~ toCard card)
          & (focusedCardsL %~ removeCard)
          & (foundCardsL %~ Map.map removeCard)
    modifiers' <- getModifiers card
    let ignoreRevelation = IgnoreRevelation `elem` modifiers'
    case toCardType card of
      EnemyType -> do
        enemyId <- getRandom
        let enemy = createEnemy card enemyId
        pushAll
          $ [Revelation iid (EnemySource enemyId) | hasRevelation card && not ignoreRevelation]
          <> [UnsetActiveCard]
        pure
          $ g'
          & (entitiesL . enemiesL . at enemyId ?~ enemy)
          & (activeCardL ?~ toCard card)
      TreacheryType -> do
        -- handles draw windows
        treacheryId <- getRandom
        let treachery = createTreachery card iid treacheryId
        -- Asset is assumed to have a revelation ability if drawn from encounter deck
        pushAll $ guard (not ignoreRevelation) *> resolve (Revelation iid $ TreacherySource treacheryId)
        pure $ g' & (entitiesL . treacheriesL . at treacheryId ?~ treachery)
      EncounterAssetType -> do
        assetId <- getRandom
        let asset = createAsset card assetId
        -- Asset is assumed to have a revelation ability if drawn from encounter deck
        pushAll $ guard (not ignoreRevelation) *> resolve (Revelation iid $ AssetSource assetId)
        pure $ g' & (entitiesL . assetsL . at assetId ?~ asset)
      EncounterEventType -> do
        eventId <- getRandom
        let owner = fromMaybe iid (toCardOwner card)
        let event' = createEvent card owner eventId
        pushAll $ guard (not ignoreRevelation) *> resolve (Revelation iid $ EventSource eventId)
        pure $ g' & (entitiesL . eventsL . at eventId ?~ event')
      LocationType -> do
        locationId <- getRandom
        let location = createLocation card locationId

        pushAll
          $ ObtainCard card.id
          : PlacedLocation (toName location) (toCardCode card) locationId
          : (guard (not ignoreRevelation) *> resolve (Revelation iid (LocationSource locationId)))
        pure $ g' & (entitiesL . locationsL . at locationId ?~ location)
      _ ->
        error
          $ "Unhandled card type: "
          <> show (toCardType card)
          <> ": "
          <> show card
  DrewTreachery iid mdeck (EncounterCard card) -> do
    treacheryId <- getRandom
    let
      treachery = overAttrs (drawnFromL .~ mdeck) $ createTreachery card iid treacheryId
      historyItem = HistoryItem HistoryTreacheriesDrawn [toCardCode treachery]
      turn = isJust $ view turnPlayerInvestigatorIdL g
      setTurnHistory = if turn then turnHistoryL %~ insertHistory iid historyItem else id

    modifiers' <- getModifiers (toTarget treachery)
    afterDraw <- checkWindows [mkAfter (Window.DrawCard iid (toCard card) Deck.EncounterDeck)]
    pushAll
      $ [GainSurge GameSource (toTarget treachery) | AddKeyword Keyword.Surge `elem` modifiers']
      <> [afterDraw, ResolveTreachery iid treacheryId]

    pure
      $ g
      & (entitiesL . treacheriesL . at treacheryId ?~ treachery)
      & (activeCardL ?~ EncounterCard card)
      & (resolvingCardL ?~ EncounterCard card)
      & (phaseHistoryL %~ insertHistory iid historyItem)
      & setTurnHistory
  ResolveTreachery iid treacheryId -> do
    treachery <- getTreachery treacheryId

    modifiers' <- getCombinedModifiers [TreacheryTarget treacheryId, CardIdTarget $ toCardId treachery]
    let ignoreRevelation = IgnoreRevelation `elem` modifiers'
    let revelation = Revelation iid (TreacherySource treacheryId)

    pushAll
      $ if ignoreRevelation
        then [toDiscardBy iid GameSource (TreacheryTarget treacheryId)]
        else
          [ When revelation
          , revelation
          , MoveWithSkillTest $ Run [After revelation, AfterRevelation iid treacheryId]
          , UnsetActiveCard
          ]
    pure $ g & (if ignoreRevelation then activeCardL .~ Nothing else id)
  MoveWithSkillTest msg' -> do
    -- No skill test showed up so just run this
    push msg'
    pure g
  MovedWithSkillTest _ msg' -> do
    -- No skill test showed up so just run this
    push msg'
    pure g
  DrewTreachery iid _ (PlayerCard card) -> do
    pid <- getPlayer iid
    sendRevelation pid (toJSON $ toCard card)
    treacheryId <- getRandom
    let treachery = createTreachery card iid treacheryId
    -- player treacheries will not trigger draw treachery windows

    modifiers' <- getModifiers (toTarget treachery)

    pushAll
      $ [RemoveCardFromHand iid (toCardId card) | hasRevelation card]
      <> [GainSurge GameSource (toTarget treachery) | AddKeyword Keyword.Surge `elem` modifiers']
      <> [ResolveTreachery iid treacheryId]

    let
      historyItem = HistoryItem HistoryTreacheriesDrawn [toCardCode treachery]
      turn = isJust $ view turnPlayerInvestigatorIdL g
      setTurnHistory =
        if turn then turnHistoryL %~ insertHistory iid historyItem else id

    pure
      $ g
      & (entitiesL . treacheriesL %~ insertMap treacheryId treachery)
      & (resolvingCardL ?~ PlayerCard card)
      & (phaseHistoryL %~ insertHistory iid historyItem)
      & setTurnHistory
  SetActiveCard c -> pure $ g & activeCardL ?~ c
  UnsetActiveCard -> pure $ g & activeCardL .~ Nothing
  AfterRevelation iid treacheryId -> do
    afterResolve <- checkAfter $ Window.ResolvesTreachery iid treacheryId
    push afterResolve
    pure $ g & activeCardL .~ Nothing
  AddCardEntity card -> do
    let
      iid = view activeInvestigatorIdL g
      setAssetPlacement :: forall a. Typeable a => a -> a
      setAssetPlacement a = case eqT @a @Asset of
        Just Refl -> overAttrs (\attrs -> attrs {assetPlacement = StillInHand iid, assetController = Just iid}) a
        Nothing -> a
      extraEntities = addCardEntityWith iid setAssetPlacement mempty card
    pure $ g & entitiesL <>~ extraEntities
  RemoveCardEntity card -> do
    case toCardType card of
      AssetType -> do
        let aid = AssetId (unsafeCardIdToUUID $ toCardId card)
        runMessage (RemoveAsset aid) g
      _ -> error "Unhandle remove card entity type"
  UseAbility _ a _ -> pure $ g & activeAbilitiesL %~ (a :)
  ResolvedAbility ab -> do
    let removedEntitiesF = if length (gameActiveAbilities g) <= 1 then actionRemovedEntitiesL .~ mempty else id
    let remainingAbilities = filter (/= ab) $ view activeAbilitiesL g
    pure
      $ g
      & (activeAbilitiesL .~ remainingAbilities)
      & removedEntitiesF
      & (entitiesL %~ (if null remainingAbilities then clearRemovedEntities else id))
  Discarded (AssetTarget aid) _ (EncounterCard _) -> do
    runMessage (RemoveAsset aid) g
  Discarded (AssetTarget aid) _source _card -> do
    maybeAsset aid >>= \case
      Nothing -> pure g
      Just _ -> runMessage (RemoveAsset aid) g
  DiscardedCost (SearchedCardTarget cid) -> do
    iid <- getActiveInvestigatorId
    card <- getCard cid
    case toCardType card of
      EventType -> do
        -- There is only one card, Astounding Revelation, that does this so we just hard code for now
        let eventId = EventId $ unsafeCardIdToUUID cid
        let event' = lookupEvent (toCardCode card) iid eventId cid
        pure
          $ g
          & (actionRemovedEntitiesL . eventsL %~ insertEntity event')
          & (inSearchEntitiesL . eventsL %~ deleteMap eventId)
      _ -> error $ "Unhandled card type: " <> show card
  Discarded (TreacheryTarget tid) _ card -> do
    treachery <- getTreachery tid
    case card of
      PlayerCard pc -> do
        let ownerId = fromJustNote "owner was not set" treachery.owner
        push $ AddToDiscard ownerId pc {pcOwner = Just ownerId}
      EncounterCard _ -> pure ()
      VengeanceCard _ -> error "Vengeance card"

    push $ RemoveTreachery tid
    pure g
  Exiled (AssetTarget aid) _ -> do
    runMessage (RemoveAsset aid) g
  Discarded (EventTarget eid) _ _ -> do
    mEvent <- getEventMaybe eid
    for_ mEvent \event' -> do
      card <- field EventCard eid
      modifiers' <- liftA2 (<>) (getModifiers $ EventTarget eid) (getModifiers $ toCardId card)
      if RemoveFromGameInsteadOfDiscard `elem` modifiers'
        then push (RemoveFromGame (EventTarget eid))
        else do
          case card of
            PlayerCard pc ->
              if PlaceOnBottomOfDeckInsteadOfDiscard `elem` modifiers'
                then do
                  let iid = eventOwner $ toAttrs event'
                  push $ PutCardOnBottomOfDeck iid (Deck.InvestigatorDeck iid) card
                else push $ AddToDiscard (eventOwner $ toAttrs event') pc
            EncounterCard _ -> error "Unhandled"
            VengeanceCard _ -> error "Vengeance card"
    pure g
  Discard miid source (TreacheryTarget tid) -> do
    mcard <- fieldMay TreacheryCard tid
    for_ mcard \card -> do
      iid <- maybe getActiveInvestigatorId pure miid
      let windows'' = windows [Window.EntityDiscarded source (toTarget tid)]
      wouldDo
        (Run $ windows'' <> [Discarded (TreacheryTarget tid) source card])
        (Window.WouldBeDiscarded (TreacheryTarget tid))
        (Window.Discarded (Just iid) source card)

    pure g
  UpdateHistory iid historyItem -> do
    let
      turn = isJust $ view turnPlayerInvestigatorIdL g
      setTurnHistory =
        if turn then turnHistoryL %~ insertHistory iid historyItem else id
    pure $ g & (phaseHistoryL %~ insertHistory iid historyItem) & setTurnHistory
  BecomeYithian iid -> do
    original <- getInvestigator iid
    let yithian = becomeYithian original
    pure $ g & (entitiesL . investigatorsL . at iid ?~ yithian)
  _ -> pure g

-- TODO: Clean this up, the found of stuff is a bit messy
preloadEntities :: HasGame m => Game -> m Game
preloadEntities g = do
  let
    investigators = view (entitiesL . investigatorsL) g
    preloadHandEntities entities investigator' = do
      asIfInHandCards <- getAsIfInHandCards (toId investigator')
      let
        setAssetPlacement :: forall a. Typeable a => a -> a
        setAssetPlacement a = case eqT @a @Asset of
          Just Refl -> overAttrs (\attrs -> attrs {assetPlacement = StillInHand (toId investigator')}) a
          Nothing -> a
        setEventPlacement :: forall a. Typeable a => a -> a
        setEventPlacement a = case eqT @a @Event of
          Just Refl -> overAttrs (\attrs -> attrs {eventPlacement = StillInHand (toId investigator')}) a
          Nothing -> a
        handEffectCards =
          filter (cdCardInHandEffects . toCardDef)
            $ investigatorHand (toAttrs investigator')
            <> asIfInHandCards
      pure
        $ if null handEffectCards
          then entities
          else
            let
              handEntities =
                foldl'
                  (addCardEntityWith (toId investigator') (setEventPlacement . setAssetPlacement))
                  defaultEntities
                  handEffectCards
             in
              insertMap (toId investigator') handEntities entities
    preloadDiscardEntities entities investigator' = do
      let
        setAssetPlacement :: forall a. Typeable a => a -> a
        setAssetPlacement a = case eqT @a @Asset of
          Just Refl -> overAttrs (\attrs -> attrs {assetPlacement = StillInDiscard (toId investigator')}) a
          Nothing -> a
        setSkillPlacement :: forall a. Typeable a => a -> a
        setSkillPlacement a = case eqT @a @Skill of
          Just Refl -> overAttrs (\attrs -> attrs {skillPlacement = StillInDiscard (toId investigator')}) a
          Nothing -> a
        discardEffectCards =
          map PlayerCard
            . filter (cdCardInDiscardEffects . toCardDef)
            $ investigatorDiscard (toAttrs investigator')
      pure
        $ if null discardEffectCards
          then entities
          else
            let
              discardEntities =
                foldl'
                  (addCardEntityWith (toId investigator') (setAssetPlacement . setSkillPlacement))
                  defaultEntities
                  discardEffectCards
             in
              insertMap (toId investigator') discardEntities entities
    foundOfElems = concat . Map.elems . view Investigator.foundCardsL . toAttrs
    searchEffectCards =
      filter (cdCardInSearchEffects . toCardDef)
        $ (concat . Map.elems $ gameFoundCards g)
        <> concatMap foundOfElems (view (entitiesL . investigatorsL) g)
  active <- getActiveInvestigatorId
  let searchEntities = foldl' (addCardEntityWith active id) defaultEntities searchEffectCards
  handEntities <- foldM preloadHandEntities mempty investigators
  discardEntities <- foldM preloadDiscardEntities mempty investigators
  pure
    $ g
      { gameInHandEntities = handEntities
      , gameInSearchEntities = searchEntities
      , gameInDiscardEntities = discardEntities
      }

-- NOTE: We need preloadEntities to be a the end because the game state is not
-- "saved" between steps here. For example if we discard a card with in discard
-- effects (See Moonstone) it won't be loaded in the environment until 1 step
-- too late.
instance RunMessage Game where
  runMessage msg g = do
    ( (modeL . here) (runMessage msg) g
        >>= (modeL . there) (runMessage msg)
        >>= entitiesL (runMessage msg)
        >>= actionRemovedEntitiesL (runMessage msg)
        >>= itraverseOf
          (inHandEntitiesL . itraversed)
          (\i -> runMessage (InHand i msg))
        >>= itraverseOf
          (inDiscardEntitiesL . itraversed)
          (\i -> runMessage (InDiscard i msg))
        >>= (inDiscardEntitiesL . itraversed) (runMessage msg)
        >>= encounterDiscardEntitiesL (runMessage msg)
        >>= inSearchEntitiesL (runMessage (InSearch msg))
        >>= (skillTestL . traverse) (runMessage msg)
        >>= (activeCostL . traverse) (runMessage msg)
        >>= runGameMessage msg
      )
      <&> handleActionDiff g
      . set enemyMovingL Nothing
      . set enemyEvadingL Nothing

runPreGameMessage :: Runner Game
runPreGameMessage msg g = case msg of
  CheckWindows ws -> do
    pushAll [Do (CheckWindows ws), EndCheckWindow]
    pure $ g & windowDepthL +~ 1 & (windowStackL %~ Just . maybe [ws] (ws :))
  EndCheckWindow -> do
    let
      windowStack =
        case fmap (drop 1) (gameWindowStack g) of
          Nothing -> Nothing
          Just [] -> Nothing
          Just ([] : xs) -> case xs of
            [] -> Nothing
            _ -> Just xs
          Just (x : xs) -> Just (x : xs)
    pure $ g & windowDepthL -~ 1 & windowStackL .~ windowStack
  ScenarioResolution _ -> do
    -- We want to empty the queue for triggering a resolution
    clearQueue
    pure $ g & (skillTestL .~ Nothing) & (skillTestResultsL .~ Nothing)
  ResetInvestigators ->
    pure
      $ g
      & (modifiersL .~ mempty)
      & (entitiesL . investigatorsL %~ map returnToBody)
      & (removedFromPlayL .~ [])
  Setup -> pure $ g & inSetupL .~ True
  StartScenario _ -> pure $ g & inSetupL .~ True
  EndSetup -> pure $ g & inSetupL .~ False
  _ -> pure g

handleActionDiff :: Game -> Game -> Game
handleActionDiff old new
  | gameInAction new = new & actionDiffL %~ (diff new old :)
  | otherwise = new
