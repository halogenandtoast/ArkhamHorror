{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Scenario.Runner
  ( module X
  ) where

import Arkham.Prelude

import Arkham.Scenario.Types as X

import Arkham.Act.Sequence
import Arkham.Act.Types ( Field (..) )
import Arkham.Asset.Types ( Field (..) )
import Arkham.CampaignLog
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.ChaosBag ()
import Arkham.Classes.Entity
import Arkham.Classes.HasQueue
import Arkham.Classes.HasTokenValue
import Arkham.Classes.Query
import Arkham.Classes.RunMessage
import Arkham.Deck qualified as Deck
import Arkham.EncounterCard.Source
import Arkham.Enemy.Types ( Field (..) )
import Arkham.Event.Types ( Field (..) )
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Helpers.Deck
import Arkham.Helpers.Scenario
import Arkham.Helpers.Window
import Arkham.Id
import Arkham.Location.Types ( Field (..) )
import Arkham.Matcher qualified as Matcher
import Arkham.Message
import Arkham.Phase
import Arkham.Projection
import Arkham.Resolution
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Treachery.Types ( Field (..) )
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window
import Arkham.Zone ( Zone )
import Arkham.Zone qualified as Zone
import Data.IntMap.Strict qualified as IntMap

instance HasTokenValue ScenarioAttrs where
  getTokenValue iid tokenFace _ = case tokenFace of
    ElderSign -> getTokenValue iid ElderSign iid
    AutoFail -> pure $ TokenValue AutoFail AutoFailModifier
    PlusOne -> pure $ TokenValue PlusOne (PositiveModifier 1)
    Zero -> pure $ TokenValue Zero (PositiveModifier 0)
    MinusOne -> pure $ TokenValue MinusOne (NegativeModifier 1)
    MinusTwo -> pure $ TokenValue MinusTwo (NegativeModifier 2)
    MinusThree -> pure $ TokenValue MinusThree (NegativeModifier 3)
    MinusFour -> pure $ TokenValue MinusFour (NegativeModifier 4)
    MinusFive -> pure $ TokenValue MinusFive (NegativeModifier 5)
    MinusSix -> pure $ TokenValue MinusSix (NegativeModifier 6)
    MinusSeven -> pure $ TokenValue MinusSeven (NegativeModifier 7)
    MinusEight -> pure $ TokenValue MinusEight (NegativeModifier 8)
    otherFace -> pure $ TokenValue otherFace NoModifier

instance RunMessage ScenarioAttrs where
  runMessage msg a =
    runScenarioAttrs msg a >>= traverseOf chaosBagL (runMessage msg)

runScenarioAttrs :: Message -> ScenarioAttrs -> GameT ScenarioAttrs
runScenarioAttrs msg a@ScenarioAttrs {..} = case msg of
  Setup -> a <$ pushEnd (Begin InvestigationPhase)
  StartCampaign -> do
    standalone <- getIsStandalone
    a <$ when standalone (push $ StartScenario scenarioId)
  InitDeck iid deck -> do
    standalone <- getIsStandalone
    a <$ when
      standalone
      do
        (deck', randomWeaknesses) <- addRandomBasicWeaknessIfNeeded deck
        weaknesses <- traverse genPlayerCard randomWeaknesses
        push $ LoadDeck iid (withDeck (<> weaknesses) deck')
  PlaceLocationMatching cardMatcher -> do
    let
      matches = filter
        (`cardMatch` (Matcher.CardWithType LocationType <> cardMatcher))
        scenarioSetAsideCards
    a <$ case matches of
      [] -> error "There were no locations with that name"
      (card : _) -> push (PlaceLocation card)
  PlaceDoomOnAgenda -> do
    agendaIds <- selectList Matcher.AnyAgenda
    case agendaIds of
      [] -> pure a
      [x] -> a <$ push (PlaceDoom (AgendaTarget x) 1)
      _ -> error "multiple agendas should be handled by the scenario"
  AdvanceAgendaDeck n _ -> do
    let
      completedAgendaStack =
        fromMaybe mempty $ lookup n scenarioCompletedAgendaStack
    (oldAgenda, agendaStack') <- case lookup n scenarioAgendaStack of
      Just (x : y : ys) -> do
        let
          fromAgendaId = AgendaId (toCardCode x)
          toAgendaId = AgendaId (toCardCode y)
        push (ReplaceAgenda fromAgendaId toAgendaId)
        pure (x, y : ys)
      _ -> error "Can not advance agenda deck"
    pure
      $ a
      & (agendaStackL . at n ?~ agendaStack')
      & (completedAgendaStackL . at n ?~ (oldAgenda : completedAgendaStack))
  ResetAgendaDeckToStage n -> do
    case lookup n scenarioCompletedAgendaStack of
      Just xs -> do
        let
          go [] as = (as, [])
          go (y : ys) as =
            if cdStage y /= Just n then go ys (y : as) else (y : as, ys)
          (prepend, remaining) = go xs []
        case (prepend, lookup n scenarioAgendaStack) of
          (toAgenda : _, Just (fromAgenda : _)) -> do
            let
              fromAgendaId = AgendaId (toCardCode fromAgenda)
              toAgendaId = AgendaId (toCardCode toAgenda)
            push (ReplaceAgenda fromAgendaId toAgendaId)
          _ -> error "Could not reset agenda deck to stage"
        pure
          $ a
          & (agendaStackL . ix n %~ (prepend <>))
          & (completedAgendaStackL . at n ?~ remaining)
      _ -> error "Invalid agenda deck to reset"
  AdvanceActDeck n _ -> do
    actStack' <- case lookup n scenarioActStack of
      Just (x : y : ys) -> do
        let
          fromActId = ActId (toCardCode x)
          toActId = ActId (toCardCode y)
        push (ReplaceAct fromActId toActId)
        pure (y : ys)
      _ -> error "Can not advance act deck"
    pure $ a & actStackL . at n ?~ actStack'
  AdvanceToAct n act newActSide _ -> do
    actStack' <- case lookup n scenarioActStack of
      Just (x : ys) -> do
        let
          fromActId = ActId (toCardCode x)
          toActId = ActId (toCardCode act)
        when
          (newActSide == B)
          (push $ AdvanceAct toActId (toSource a) AdvancedWithOther)
        push (ReplaceAct fromActId toActId)
        pure $ filter
          (\c ->
            (cdStage c /= cdStage act)
              || (cdCardCode c `cardCodeExactEq` cdCardCode act)
          )
          ys
      _ -> error "Can not advance act deck"
    pure $ a & actStackL . at n ?~ actStack'

  Discard (ActTarget _) -> pure $ a & actStackL .~ mempty
  -- See: Vengeance Awaits / The Devourer Below - right now the assumption
  -- is that the act deck has been replaced.
  CheckForRemainingInvestigators -> do
    investigatorIds <- selectList Matcher.UneliminatedInvestigator
    a <$ when
      (null investigatorIds && not scenarioInResolution)
      (push $ HandleNoRemainingInvestigators
        scenarioNoRemainingInvestigatorsHandler
      )
  AllInvestigatorsResigned -> a <$ push
    (HandleNoRemainingInvestigators scenarioNoRemainingInvestigatorsHandler)
  SetNoRemainingInvestigatorsHandler target -> do
    pure $ a & noRemainingInvestigatorsHandlerL .~ target
  HandleNoRemainingInvestigators target | isTarget a target -> do
    clearQueue @Message
    push (ScenarioResolution NoResolution)
    pure $ a & inResolutionL .~ True -- must set to avoid redundancy when scenario kills investigator
  InvestigatorWhenEliminated _ iid -> do
    whenMsg <- checkWindows
      [Window Timing.When (Window.InvestigatorEliminated iid)]
    afterMsg <- checkWindows
      [Window Timing.When (Window.InvestigatorEliminated iid)]
    a <$ pushAll
      [ whenMsg
      , InvestigatorPlaceAllCluesOnLocation iid
      , InvestigatorEliminated iid
      , CheckForRemainingInvestigators
      , afterMsg
      ]
  Remember logKey -> pure $ a & logL %~ insertSet logKey
  ResolveToken _drawnToken token _iid | token == AutoFail ->
    a <$ push FailSkillTest
  EndOfScenario mNextCampaignStep -> do
    clearQueue @Message
    standalone <- getIsStandalone
    a <$ push
      (if standalone
        then GameOver
        else maybe
          (NextCampaignStep Nothing)
          (CampaignStep . Just)
          mNextCampaignStep
      )
  ScenarioResolution _ ->
    error "The scenario should specify what to do for no resolution"
  LookAtTopOfDeck _ ScenarioDeckTarget _ ->
    error "The scenario should handle looking at the top of the scenario deck"
  DrawFromScenarioDeck iid key target n -> case lookup key scenarioDecks of
    Just [] -> pure a
    Just xs -> do
      let (drew, rest) = splitAt n xs
      push (DrewFromScenarioDeck iid key target drew)
      pure $ a & decksL . at key ?~ rest
    _ ->
      error
        $ "Invalid scenario deck key "
        <> show key
        <> ", could not find deck in scenario"
  DrawRandomFromScenarioDeck iid key target n ->
    case lookup key scenarioDecks of
      Just [] -> pure a
      Just xs -> do
        (drew, rest) <- splitAt n <$> shuffleM xs
        push (DrewFromScenarioDeck iid key target drew)
        pure $ a & decksL . at key ?~ rest
      _ ->
        error
          $ "Invalid scenario deck key "
          <> show key
          <> ", could not find deck in scenario"
  ShuffleScenarioDeckIntoEncounterDeck key -> case lookup key scenarioDecks of
    Just [] -> pure a
    Just xs -> do
      push $ ShuffleCardsIntoDeck Deck.EncounterDeck xs
      pure $ a & decksL %~ deleteMap key
    _ ->
      error
        $ "Invalid scenario deck key "
        <> show key
        <> ", could not find deck in scenario"
  SetScenarioDeck key cards -> pure $ a & (decksL . at key ?~ cards)
  AddCardToScenarioDeck key card -> case lookup key scenarioDecks of
    Just cards -> pure $ a & (decksL . at key ?~ card : cards)
    _ ->
      error
        $ "Invalid scenario deck key "
        <> show key
        <> ", could not find deck in scenario"
  ChooseRandomLocation target exclusions -> do
    locationIds <-
      setToList . (`difference` exclusions) <$> select Matcher.Anywhere
    case nonEmpty locationIds of
      Nothing -> error "no locations?"
      Just lids -> do
        randomLocationId <- sample lids
        msgs <- windows [Window.ChosenRandomLocation randomLocationId]
        a <$ pushAll (msgs <> [ChosenRandomLocation target randomLocationId])
  PlaceLocation card -> pure $ a & setAsideCardsL %~ delete card
  CreateWeaknessInThreatArea card _ -> pure $ a & setAsideCardsL %~ delete card
  PutCardOnTopOfDeck _ Deck.EncounterDeck card -> case card of
    EncounterCard ec -> do
      let
        encounterDeck = flip withDeck scenarioEncounterDeck
          $ \cards -> ec : deleteFirst ec cards
      pure
        $ a
        & (setAsideCardsL %~ deleteFirstMatch (== card))
        & (encounterDeckL .~ encounterDeck)
    PlayerCard _ -> error "can not place player card on top of encounter deck"
  PutCardOnBottomOfDeck _ Deck.EncounterDeck card -> case card of
    EncounterCard ec -> do
      let encounterDeck = withDeck (<> [ec]) scenarioEncounterDeck
      pure
        $ a
        & (setAsideCardsL %~ deleteFirstMatch (== card))
        & (encounterDeckL .~ encounterDeck)
    PlayerCard _ ->
      error "can not place player card on bottom of encounter deck"
  PutCardOnTopOfDeck _ (Deck.ScenarioDeckByKey deckKey) card -> do
    let
      deck = fromMaybe [] $ view (decksL . at deckKey) a
      deck' = card : deleteFirst card deck
    pure
      $ a
      & (setAsideCardsL %~ deleteFirstMatch (== card))
      & (decksL . at deckKey ?~ deck')
  PutCardOnBottomOfDeck _ (Deck.ScenarioDeckByKey deckKey) card -> do
    let
      deck = fromMaybe [] $ view (decksL . at deckKey) a
      deck' = deleteFirst card deck <> [card]
    pure
      $ a
      & (setAsideCardsL %~ deleteFirstMatch (== card))
      & (decksL . at deckKey ?~ deck')
  AddToEncounterDeck card -> do
    encounterDeck <- withDeckM (shuffleM . (card :)) scenarioEncounterDeck
    pure
      $ a
      & (setAsideCardsL %~ deleteFirstMatch (== EncounterCard card))
      & (encounterDeckL .~ encounterDeck)
  AddToEncounterDiscard ec -> do
    pure $ a & discardL %~ (ec :)
  AddToVictory (EventTarget eid) -> do
    card <- field EventCard eid
    pure $ a & (victoryDisplayL %~ (card :))
  AddToVictory (ActTarget aid) -> do
    card <- field ActCard aid
    pure $ a & (victoryDisplayL %~ (card :))
  AddToVictory (EnemyTarget eid) -> do
    card <- field EnemyCard eid
    pure $ a & (victoryDisplayL %~ (card :))
  DefeatedAddToVictory (EnemyTarget eid) -> do
    card <- field EnemyCard eid
    pure $ a & (victoryDisplayL %~ (card :))
  Discarded (EnemyTarget eid) _ -> do
    card <- field EnemyCard eid
    case card of
      PlayerCard _ -> pure a
      EncounterCard ec -> pure $ a & discardL %~ (ec :)
  CreateAssetAt card _ -> do
    pure $ a & setAsideCardsL %~ deleteFirstMatch (== card)
  AttachStoryTreacheryTo card _ -> do
    pure $ a & setAsideCardsL %~ deleteFirstMatch (== card)
  CreateEnemyAt card _ _ -> do
    pure $ a & setAsideCardsL %~ deleteFirstMatch (== card)
  PlaceUnderneath AgendaDeckTarget cards -> do
    pure $ a & cardsUnderAgendaDeckL <>~ cards
  PlaceUnderneath ActDeckTarget cards -> do
    for_ cards $ \card -> pushAll =<< splitWithWindows
      (PlacedUnderneath ActDeckTarget card)
      [Window.PlaceUnderneath ActDeckTarget card]
    pure a
  PlacedUnderneath ActDeckTarget card -> do
    pure $ a & cardsUnderActDeckL %~ (card :)
  PlaceNextTo ActDeckTarget cards -> do
    pure $ a & cardsNextToActDeckL <>~ cards
  ShuffleCardsIntoDeck Deck.EncounterDeck cards -> do
    let
      encounterCards = mapMaybe (preview _EncounterCard) cards
      filterCards = filter (`notElem` cards)
    deck' <- withDeckM (shuffleM . (<> encounterCards)) scenarioEncounterDeck
    pure
      $ a
      & (cardsUnderAgendaDeckL %~ filterCards)
      & (cardsUnderActDeckL %~ filterCards)
      & (cardsNextToActDeckL %~ filterCards)
      & (cardsUnderScenarioReferenceL %~ filterCards)
      & (setAsideCardsL %~ filterCards)
      & (encounterDeckL .~ deck')
  RequestSetAsideCard target cardCode -> do
    let
      (before, rest) = break ((== cardCode) . toCardCode) scenarioSetAsideCards
    case rest of
      [] -> error "requested a card that is not set aside"
      (x : xs) -> do
        push (RequestedSetAsideCard target x)
        pure $ a & setAsideCardsL .~ (before <> xs)
  TakeControlOfSetAsideAsset _ card -> do
    let
      cardCode = toCardCode card
      (before, rest) = break ((== cardCode) . toCardCode) scenarioSetAsideCards
    case rest of
      [] -> pure a
      (_ : xs) -> pure $ a & setAsideCardsL .~ (before <> xs)
  ReadStory iid story' -> do
    push
      (chooseOne iid [CardLabel (cdCardCode story') [ResolveStory iid story']])
    pure $ a & cardsUnderScenarioReferenceL %~ filter ((/= story') . toCardDef)
  SetActDeck -> do
    case a ^. actStackL . at 1 of
      Just (x : _) -> push (AddAct x)
      _ -> pure ()
    pure a
  SetAgendaDeck -> do
    let ks = sortOn Down $ a ^. agendaStackL . to IntMap.keys
    for_ ks $ \k -> do
      case a ^. agendaStackL . at k of
        Just (x : _) -> push (AddAgenda k x)
        _ -> pure ()
    pure a
  AddCampaignCardToDeck iid cardDef -> do
    standalone <- getIsStandalone
    if standalone
      then do
        card <- genPlayerCard cardDef
        push
          (ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [PlayerCard card])
        pure $ a & storyCardsL %~ insertWith
          (<>)
          iid
          [card { pcOwner = Just iid }]
      else pure a
  LookAtTopOfDeck iid EncounterDeckTarget n -> do
    let cards = map EncounterCard . take n $ unDeck scenarioEncounterDeck
    a <$ pushAll
      [FocusCards cards, chooseOne iid [Label "Continue" [UnfocusCards]]]
  MoveTopOfDeckToBottom _ Deck.EncounterDeck n -> do
    let (cards, deck) = splitAt n (unDeck scenarioEncounterDeck)
    pure $ a & encounterDeckL .~ Deck (deck <> cards)
  Discard (TreacheryTarget tid) -> do
    card <- field TreacheryCard tid
    case card of
      PlayerCard _ -> pure a
      EncounterCard ec -> pure $ a & discardL %~ (ec :)
  InvestigatorDoDrawEncounterCard iid -> case unDeck scenarioEncounterDeck of
    [] -> do
      when (notNull scenarioDiscard) $ pushAll
        [ShuffleEncounterDiscardBackIn, InvestigatorDrawEncounterCard iid]
      pure a
      -- This case should not happen but this safeguards against it
    (card : encounterDeck) -> do
      when (null encounterDeck) (push ShuffleEncounterDiscardBackIn)
      pushAll [UnsetActiveCard, InvestigatorDrewEncounterCard iid card]
      pure $ a & (encounterDeckL .~ Deck encounterDeck)
  Search iid source EncounterDeckTarget cardSources _traits foundStrategy -> do
    let
      foundCards :: HashMap Zone [Card] = foldl'
        (\hmap (cardSource, _) -> case cardSource of
          Zone.FromDeck -> insertWith
            (<>)
            Zone.FromDeck
            (map EncounterCard $ unDeck scenarioEncounterDeck)
            hmap
          Zone.FromTopOfDeck n -> insertWith
            (<>)
            Zone.FromDeck
            (map EncounterCard . take n $ unDeck scenarioEncounterDeck)
            hmap
          Zone.FromDiscard -> insertWith
            (<>)
            Zone.FromDiscard
            (map EncounterCard scenarioDiscard)
            hmap
          other -> error $ mconcat ["Zone ", show other, " not yet handled"]
        )
        mempty
        cardSources
      encounterDeck = filter
        ((`notElem` findWithDefault [] Zone.FromDeck foundCards) . EncounterCard
        )
        (unDeck scenarioEncounterDeck)
      targetCards = concat $ toList foundCards
    push $ EndSearch iid source EncounterDeckTarget cardSources
    case foundStrategy of
      DrawFound who n -> do
        let
          choices =
            [ TargetLabel
                (CardIdTarget $ toCardId card)
                [InvestigatorDrewEncounterCard who card]
            | card <- mapMaybe (preview _EncounterCard) targetCards
            ]
        push
          (chooseN iid n
          $ if null choices then [Label "No cards found" []] else choices
          )
      DeferSearchedToTarget searchTarget -> do
        push $ if null targetCards
          then chooseOne
            iid
            [Label "No cards found" [SearchNoneFound iid searchTarget]]
          else SearchFound iid searchTarget Deck.EncounterDeck targetCards
      PlayFound{} -> error "PlayFound is not a valid EncounterDeck strategy"
      ReturnCards -> pure ()

    push (FoundCards foundCards)

    pure $ a & (encounterDeckL .~ Deck encounterDeck)
  Discarded (AssetTarget _) (EncounterCard ec) ->
    -- TODO: determine why this was only specified for Asset
    pure $ a & discardL %~ (ec :)
  ResignWith (AssetTarget aid) -> do
    cardCode <- field AssetCardCode aid
    pure $ a & resignedCardCodesL %~ (cardCode :)
  RemoveFromEncounterDiscard ec -> pure $ a & discardL %~ filter (/= ec)
  ShuffleEncounterDiscardBackIn -> do
    encounterDeck <- withDeckM
      (shuffleM . (<> scenarioDiscard))
      scenarioEncounterDeck
    pure $ a & encounterDeckL .~ encounterDeck & discardL .~ mempty
  ShuffleAllInEncounterDiscardBackIn cardCode -> do
    let
      (toShuffleBackIn, discard) =
        partition ((== cardCode) . toCardCode) scenarioDiscard
    encounterDeck <- withDeckM
      (shuffleM . (<> toShuffleBackIn))
      scenarioEncounterDeck
    pure $ a & encounterDeckL .~ encounterDeck & discardL .~ discard
  ShuffleBackIntoEncounterDeck (EnemyTarget eid) -> do
    card <- field EnemyCard eid
    case card of
      EncounterCard card' -> do
        push $ RemoveEnemy eid
        encounterDeck <- withDeckM (shuffleM . (card' :)) scenarioEncounterDeck
        pure $ a & encounterDeckL .~ encounterDeck
      _ -> error "must be encounter card"
  ShuffleBackIntoEncounterDeck (LocationTarget lid) -> do
    card <- field LocationCard lid
    case card of
      EncounterCard card' -> do
        pushAll $ resolve (RemoveLocation lid)
        encounterDeck <- withDeckM (shuffleM . (card' :)) scenarioEncounterDeck
        pure $ a & encounterDeckL .~ encounterDeck
      _ -> error "must be encounter card"
  DiscardEncounterUntilFirst source matcher -> do
    let
      (discards, remainingDeck) =
        break (`cardMatch` matcher) (unDeck scenarioEncounterDeck)
    case remainingDeck of
      [] -> do
        push (RequestedEncounterCard source Nothing)
        encounterDeck <- shuffleM (discards <> scenarioDiscard)
        pure $ a & encounterDeckL .~ Deck encounterDeck & discardL .~ mempty
      (x : xs) -> do
        push (RequestedEncounterCard source (Just x))
        pure $ a & encounterDeckL .~ Deck xs & discardL %~ (reverse discards <>)
  FoundAndDrewEncounterCard iid cardSource card -> do
    let
      cardId = toCardId card
      discard = case cardSource of
        FromDiscard -> filter ((/= cardId) . toCardId) scenarioDiscard
        _ -> scenarioDiscard
      encounterDeck = case cardSource of
        FromEncounterDeck ->
          filter ((/= cardId) . toCardId) (unDeck scenarioEncounterDeck)
        _ -> unDeck scenarioEncounterDeck
    shuffled <- shuffleM encounterDeck
    push (InvestigatorDrewEncounterCard iid card)
    pure $ a & (encounterDeckL .~ Deck shuffled) & (discardL .~ discard)
  FoundEncounterCardFrom iid target cardSource card -> do
    let
      cardId = toCardId card
      discard = case cardSource of
        FromDiscard -> filter ((/= cardId) . toCardId) scenarioDiscard
        _ -> scenarioDiscard
      encounterDeck = case cardSource of
        FromEncounterDeck ->
          filter ((/= cardId) . toCardId) (unDeck scenarioEncounterDeck)
        _ -> unDeck scenarioEncounterDeck
    shuffled <- shuffleM encounterDeck
    push (FoundEncounterCard iid target card)
    pure $ a & (encounterDeckL .~ Deck shuffled) & (discardL .~ discard)
  FindEncounterCard iid target matcher -> do
    let
      matchingDiscards = filter (`cardMatch` matcher) scenarioDiscard
      matchingDeckCards =
        filter (`cardMatch` matcher) (unDeck scenarioEncounterDeck)
      matchingVoidEnemies = case matcher of
        -- TODO: FIX
        -- CardWithCardCode cardCode ->
        --   filter ((== cardCode) . toCardCode) . toList $ g ^. enemiesInVoidL
        _ -> []

    when
        (notNull matchingDiscards
        || notNull matchingDeckCards
        || notNull matchingVoidEnemies
        )
      $ push
      $ chooseOne iid
      $ [ TargetLabel
            (CardIdTarget $ toCardId card)
            [FoundEncounterCardFrom iid target FromDiscard card]
        | card <- matchingDiscards
        ]
      <> [ TargetLabel
             (CardIdTarget $ toCardId card)
             [FoundEncounterCardFrom iid target FromEncounterDeck card]
         | card <- matchingDeckCards
         ]
      <> [ targetLabel eid [FoundEnemyInVoid iid target eid]
         | eid <- matchingVoidEnemies
         ]

    -- TODO: show where focused cards are from

    push
      $ FocusCards
      $ map EncounterCard matchingDeckCards
      <> map EncounterCard matchingDiscards
         -- <> map toCard matchingVoidEnemies
    pure a
  FindAndDrawEncounterCard iid matcher -> do
    let
      matchingDiscards = filter (`cardMatch` matcher) scenarioDiscard
      matchingDeckCards =
        filter (`cardMatch` matcher) (unDeck scenarioEncounterDeck)

    push
      $ chooseOne iid
      $ [ TargetLabel
            (CardIdTarget $ toCardId card)
            [FoundAndDrewEncounterCard iid FromDiscard card]
        | card <- matchingDiscards
        ]
      <> [ TargetLabel
             (CardIdTarget $ toCardId card)
             [FoundAndDrewEncounterCard iid FromEncounterDeck card]
         | card <- matchingDeckCards
         ]
    -- TODO: show where focused cards are from
    push
      $ FocusCards
      $ map EncounterCard matchingDeckCards
      <> map EncounterCard matchingDiscards
    pure a
  DrawEncounterCards target n -> do
    let (cards, encounterDeck) = splitAt n $ unDeck scenarioEncounterDeck
    push (RequestedEncounterCards target cards)
    pure $ a & encounterDeckL .~ Deck encounterDeck
  DiscardTopOfEncounterDeck iid n mtarget ->
    a <$ push (DiscardTopOfEncounterDeckWithDiscardedCards iid n mtarget [])
  DiscardTopOfEncounterDeckWithDiscardedCards iid 0 mtarget cards ->
    a <$ case mtarget of
      Nothing -> pure ()
      Just target -> push (DiscardedTopOfEncounterDeck iid cards target)
  DiscardTopOfEncounterDeckWithDiscardedCards iid n mtarget discardedCards ->
    case unDeck scenarioEncounterDeck of
      [] -> do
        push $ DiscardTopOfEncounterDeckWithDiscardedCards
          iid
          0
          mtarget
          discardedCards
        pure a
      (card : cards) -> do
        pushAll
          $ Discarded (InvestigatorTarget iid) (EncounterCard card)
          : [ ShuffleEncounterDiscardBackIn | null cards ]
          <> [ DiscardTopOfEncounterDeckWithDiscardedCards
                 iid
                 (n - 1)
                 mtarget
                 (card : discardedCards)
             ]
        pure $ a & discardL %~ (card :) & encounterDeckL .~ Deck cards
  CreatedEnemyAt enemyId _ _ -> do
    card <- field EnemyCard enemyId
    pure $ a & (victoryDisplayL %~ delete card)
  SetEncounterDeck encounterDeck -> pure $ a & encounterDeckL .~ encounterDeck
  RemoveAllCopiesOfCardFromGame _ cCode ->
    pure $ a & setAsideCardsL %~ filter ((/= cCode) . toCardCode)
  Record key -> do
    isStandalone <- getIsStandalone
    if isStandalone
      then pure $ a & standaloneCampaignLogL . recorded %~ insertSet key
      else pure a
  ShuffleDeck (Deck.ScenarioDeckByKey deckKey) -> do
    deck' <- shuffleM $ fromMaybe [] (view (decksL . at deckKey) a)
    pure $ a & decksL . at deckKey ?~ deck'
  ShuffleCardsIntoDeck (Deck.ScenarioDeckByKey deckKey) cards -> do
    deck' <- shuffleM $ cards <> fromMaybe [] (view (decksL . at deckKey) a)
    pure $ a & decksL . at deckKey ?~ deck'
  RemoveAllDoomFromPlay matchers -> do
    let Matcher.RemoveDoomMatchers {..} = matchers
    locations <- selectListMap
      LocationTarget
      (removeDoomLocations <> Matcher.LocationWithAnyDoom)
    investigators <- selectListMap InvestigatorTarget removeDoomInvestigators
    enemies <- selectListMap
      EnemyTarget
      (removeDoomEnemies <> Matcher.EnemyWithAnyDoom)
    assets <- selectListMap
      AssetTarget
      (removeDoomAssets <> Matcher.AssetWithAnyDoom)
    acts <- selectListMap ActTarget removeDoomActs
    agendas <- selectListMap
      AgendaTarget
      (removeDoomAgendas <> Matcher.AgendaWithAnyDoom)
    treacheries <- selectListMap TreacheryTarget removeDoomTreacheries
    events <- selectListMap EventTarget removeDoomEvents
    skills <- selectListMap SkillTarget removeDoomSkills
    pushAll
      [ RemoveAllDoom target
      | target <-
        locations
        <> investigators
        <> enemies
        <> assets
        <> acts
        <> agendas
        <> treacheries
        <> events
        <> skills
      ]
    pure a
  _ -> pure a
