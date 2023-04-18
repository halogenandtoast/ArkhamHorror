{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Scenario.Runner
  ( module X
  ) where

import Arkham.Prelude

import Arkham.Helpers.Message as X
import Arkham.Scenario.Types as X

import Arkham.Act.Sequence qualified as Act
import Arkham.Act.Types ( Field (..) )
import Arkham.Agenda.Sequence qualified as Agenda
import Arkham.Agenda.Types ( Field (..) )
import Arkham.Asset.Types ( Field (..) )
import Arkham.CampaignLog
import Arkham.Card
import Arkham.ChaosBag ()
import Arkham.Classes.Entity
import Arkham.Classes.GameLogger
import Arkham.Classes.HasQueue
import Arkham.Classes.HasTokenValue
import Arkham.Classes.Query
import Arkham.Classes.RunMessage
import Arkham.Deck qualified as Deck
import Arkham.DefeatedBy
import Arkham.EncounterCard.Source
import Arkham.Enemy.Types ( Field (..) )
import Arkham.Event.Types ( Field (..) )
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Helpers.Deck
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Helpers.Window
import Arkham.Id
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Types ( Field (..) )
import Arkham.Matcher qualified as Matcher
import Arkham.Message
import Arkham.Phase
import Arkham.Projection
import Arkham.Resolution
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Treachery.Types ( Field (..) )
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window
import Arkham.Zone ( Zone )
import Arkham.Zone qualified as Zone
import Data.HashMap.Strict qualified as HashMap
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
  ResetGame -> do
    standalone <- getIsStandalone
    when standalone $ do
      investigatorIds <- allInvestigatorIds
      for_ investigatorIds $ \iid -> do
        deck <- fieldMap InvestigatorDeck unDeck iid
        hand <- mapMaybe (preview _PlayerCard) <$> field InvestigatorHand iid
        discard <- field InvestigatorDiscard iid
        let deck' = deck <> hand <> discard
        push $ LoadDeck iid (Deck deck')
    pure a
  Setup -> a <$ pushAllEnd [BeginGame, Begin InvestigationPhase]
  StartCampaign -> do
    standalone <- getIsStandalone
    a <$ when standalone (push $ StartScenario scenarioId)
  InitDeck iid deck -> do
    standalone <- getIsStandalone
    if standalone
      then do
        (deck', randomWeaknesses) <- addRandomBasicWeaknessIfNeeded deck
        weaknesses <- traverse genPlayerCard randomWeaknesses
        let
          mentalTrauma = getSum $ foldMap
            (Sum . fromMaybe 0 . cdPurchaseMentalTrauma . toCardDef)
            deck'
        pushAll
          $ LoadDeck iid (withDeck (<> weaknesses) deck')
          : [ SufferTrauma iid 0 mentalTrauma | mentalTrauma > 0 ]
        pure $ a & playerDecksL %~ insertMap iid deck'
      else pure a
  PlaceLocationMatching cardMatcher -> do
    let
      matches = filter
        (`cardMatch` (Matcher.CardWithType LocationType <> cardMatcher))
        scenarioSetAsideCards
    locationId <- getRandom
    a <$ case matches of
      [] -> error "There were no locations with that name"
      (card : _) -> push (PlaceLocation locationId card)
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
        let fromAgendaId = AgendaId (toCardCode x)
        push (ReplaceAgenda fromAgendaId y)
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
          go (y : ys) as = if cdStage (toCardDef y) /= Just n
            then go ys (y : as)
            else (y : as, ys)
          (prepend, remaining) = go xs []
        case (prepend, lookup n scenarioAgendaStack) of
          (toAgenda : _, Just (fromAgenda : _)) -> do
            let fromAgendaId = AgendaId (toCardCode fromAgenda)
            push (ReplaceAgenda fromAgendaId toAgenda)
          _ -> error "Could not reset agenda deck to stage"
        pure
          $ a
          & (agendaStackL . ix n %~ (prepend <>))
          & (completedAgendaStackL . at n ?~ remaining)
      _ -> error "Invalid agenda deck to reset"
  AdvanceActDeck n _ -> do
    let
      completedActStack = fromMaybe mempty $ lookup n scenarioCompletedActStack
    (oldAct, actStack') <- case lookup n scenarioActStack of
      Just (x : y : ys) -> do
        let fromActId = ActId (toCardCode x)
        push (ReplaceAct fromActId y)
        pure (x, y : ys)
      _ -> error "Can not advance act deck"
    pure
      $ a
      & actStackL
      . at n
      ?~ actStack'
      & (completedActStackL . at n ?~ (oldAct : completedActStack))
  SetCurrentActDeck n stack@(current : _) -> do
    actIds <- selectList $ Matcher.ActWithDeckId n
    pushAll
      $ [ Discard GameSource (ActTarget actId) | actId <- actIds ]
      <> [AddAct n current]
    pure
      $ a
      & (actStackL . at n ?~ stack)
      & (setAsideCardsL %~ filter (`notElem` stack))
  SetCurrentAgendaDeck n stack@(current : _) -> do
    agendaIds <- selectList $ Matcher.AgendaWithDeckId n
    pushAll
      $ [ Discard GameSource (AgendaTarget agendaId) | agendaId <- agendaIds ]
      <> [AddAgenda n current]
    pure
      $ a
      & (agendaStackL . at n ?~ stack)
      & (setAsideCardsL %~ filter (`notElem` stack))
  AdvanceToAct n actDef newActSide _ -> do
    let
      completedActStack = fromMaybe mempty $ lookup n scenarioCompletedActStack
    (oldAct, actStack') <- case lookup n scenarioActStack of
      Just (x : ys) -> do
        let fromActId = ActId (toCardCode x)
        case find (`isCard` actDef) ys of
          Nothing -> error $ "Missing act: " <> show actDef
          Just toAct -> do
            let toActId = ActId (toCardCode toAct)
            when
              (newActSide == Act.B)
              (push $ AdvanceAct toActId (toSource a) AdvancedWithOther)
            push (ReplaceAct fromActId toAct)
            pure
              ( x
              , filter
                (\c ->
                  (cdStage (toCardDef c) /= cdStage actDef)
                    || (toCardCode c `cardCodeExactEq` toCardCode actDef)
                )
                ys
              )
      _ -> error "Can not advance act deck"
    pure
      $ a
      & (actStackL . at n ?~ actStack')
      & (completedActStackL . at n ?~ oldAct : completedActStack)
  ResetActDeckToStage n -> do
    case lookup n scenarioCompletedActStack of
      Just xs -> do
        let
          go [] as = (as, [])
          go (y : ys) as = if cdStage (toCardDef y) /= Just n
            then go ys (y : as)
            else (y : as, ys)
          (prepend, remaining) = go xs []
        case (prepend, lookup n scenarioActStack) of
          (toAct : _, Just (fromAct : _)) -> do
            let fromActId = ActId (toCardCode fromAct)
            push (ReplaceAct fromActId toAct)
          _ -> error "Could not reset act deck to stage"
        pure
          $ a
          & (actStackL . ix n %~ (prepend <>))
          & (completedActStackL . at n ?~ remaining)
      _ -> error "Invalid act deck to reset"
  AdvanceToAgenda n agendaDef newAgendaSide _ -> do
    agendaStack' <- case lookup n scenarioAgendaStack of
      Just (x : ys) -> do
        let fromAgendaId = AgendaId (toCardCode x)
        case find (`isCard` agendaDef) ys of
          Nothing -> error $ "Missing agenda: " <> show agendaDef
          Just toAgenda -> do
            let toAgendaId = AgendaId (toCardCode toAgenda)
            when (newAgendaSide == Agenda.B) $ push $ AdvanceAgenda toAgendaId
            push (ReplaceAgenda fromAgendaId toAgenda)
            -- filter the stack so only agendas with higher stages are left
            pure $ filter
              (\c ->
                (fromMaybe
                    False
                    (liftA2 (>) (cdStage $ toCardDef c) (cdStage agendaDef))
                  )
                  || (toCardCode c `cardCodeExactEq` toCardCode agendaDef)
              )
              ys
      _ -> error "Can not advance agenda deck"
    pure $ a & agendaStackL . at n ?~ agendaStack'

  Discard _ (ActTarget _) -> pure $ a & actStackL .~ mempty
  -- See: Vengeance Awaits / The Devourer Below - right now the assumption
  -- is that the act deck has been replaced.
  CheckForRemainingInvestigators -> do
    investigatorIds <- selectList Matcher.UneliminatedInvestigator
    when (null investigatorIds && not scenarioInResolution)
      $ push
      $ HandleNoRemainingInvestigators scenarioNoRemainingInvestigatorsHandler
    pure a
  AllInvestigatorsResigned -> a <$ push
    (HandleNoRemainingInvestigators scenarioNoRemainingInvestigatorsHandler)
  SetNoRemainingInvestigatorsHandler target -> do
    pure $ a & noRemainingInvestigatorsHandlerL .~ target
  HandleNoRemainingInvestigators target | isTarget a target -> do
    clearQueue
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
  Remember logKey -> do
    send $ "Remember \"" <> format logKey <> "\""
    pure $ a & logL %~ insertSet logKey
  ScenarioCountIncrementBy logKey n ->
    pure $ a & countsL %~ HashMap.alter (Just . maybe n (+ n)) logKey
  ScenarioCountDecrementBy logKey n ->
    pure
      $ a
      & countsL
      %~ HashMap.alter (Just . max 0 . maybe 0 (subtract n)) logKey
  ResolveToken _drawnToken token iid -> do
    TokenValue _ tokenModifier <- getTokenValue iid token ()
    when (tokenModifier == AutoFailModifier) $ push FailSkillTest
    pure a
  EndOfScenario mNextCampaignStep -> do
    clearQueue
    standalone <- getIsStandalone
    push $ if standalone then GameOver else NextCampaignStep mNextCampaignStep
    pure a
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
  RemoveCardFromScenarioDeck key card ->
    pure $ a & (decksL . ix key %~ filter (/= card))
  ChooseRandomLocation target exclusions -> do
    locationIds <-
      setToList . (`difference` exclusions) <$> select Matcher.Anywhere
    case nonEmpty locationIds of
      Nothing -> error "no locations?"
      Just lids -> do
        randomLocationId <- sample lids
        msgs <- windows [Window.ChosenRandomLocation randomLocationId]
        a <$ pushAll (msgs <> [ChosenRandomLocation target randomLocationId])
  SetCardAside card -> pure $ a & setAsideCardsL %~ (card:)
  PlaceLocation _ card -> pure $ a & setAsideCardsL %~ delete card
  ReplaceLocation _ card _ -> pure $ a & setAsideCardsL %~ delete card
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
    VengeanceCard _ -> error "vengeance card"
  PutCardOnBottomOfDeck _ Deck.EncounterDeck card -> case card of
    EncounterCard ec -> do
      let encounterDeck = withDeck (<> [ec]) scenarioEncounterDeck
      pure
        $ a
        & (setAsideCardsL %~ deleteFirstMatch (== card))
        & (encounterDeckL .~ encounterDeck)
    PlayerCard _ ->
      error "can not place player card on bottom of encounter deck"
    VengeanceCard _ -> error "vengeance card"
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
    pure
      $ a
      & (discardL %~ (ec :))
      & (encounterDeckL %~ withDeck (filter (/= ec)))
      & (victoryDisplayL %~ filter (/= EncounterCard ec))
      & (setAsideCardsL %~ filter (/= EncounterCard ec))
  AddToVictory (EventTarget eid) -> do
    card <- field EventCard eid
    pure $ a & (victoryDisplayL %~ (card :))
  AddToVictory (TreacheryTarget tid) -> do
    card <- field TreacheryCard tid
    pure $ a & (victoryDisplayL %~ (card :))
  AddToVictory (ActTarget aid) -> do
    card <- field ActCard aid
    pure $ a & (victoryDisplayL %~ (card :))
  AddToVictory (AgendaTarget aid) -> do
    card <- field AgendaCard aid
    pure $ a & (victoryDisplayL %~ (card :))
  AddToVictory (EnemyTarget eid) -> do
    card <- field EnemyCard eid
    pure $ a & (victoryDisplayL %~ (card :))
  AddToVictory (LocationTarget lid) -> do
    card <- field LocationCard lid
    pure $ a & (victoryDisplayL %~ (card :))
  DefeatedAddToVictory (EnemyTarget eid) -> do
    card <- field EnemyCard eid
    pure $ a & (victoryDisplayL %~ (card :))
  Discarded (EnemyTarget eid) _ _ -> do
    card <- field EnemyCard eid
    case card of
      PlayerCard _ -> pure a
      EncounterCard ec -> pure $ a & discardL %~ (ec :)
      VengeanceCard _ -> error "vengeance card"
  CreateAssetAt _ card _ -> do
    pure $ a & setAsideCardsL %~ deleteFirstMatch (== card)
  AttachStoryTreacheryTo card _ -> do
    pure $ a & setAsideCardsL %~ deleteFirstMatch (== card)
  CreateEnemyAt _ card _ _ -> do
    pure
      $ a
      & (setAsideCardsL %~ deleteFirstMatch (== card))
      & (victoryDisplayL %~ filter (/= card))
      & (encounterDeckL %~ withDeck (filter ((/= card) . EncounterCard)))
      & (discardL %~ filter ((/= card) . EncounterCard))
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
  ShuffleCardsIntoDeck Deck.EncounterDeck [] -> do
    pure a
  ShuffleCardsIntoDeck Deck.EncounterDeck cards -> do
    let
      encounterCards = mapMaybe (preview _EncounterCard) cards
      filterCards = filter (`notElem` cards)
    deck' <- withDeckM (shuffleM . (<> encounterCards) . filter (`notElem` encounterCards)) scenarioEncounterDeck
    pure
      $ a
      & (cardsUnderAgendaDeckL %~ filterCards)
      & (cardsUnderActDeckL %~ filterCards)
      & (cardsNextToActDeckL %~ filterCards)
      & (cardsUnderScenarioReferenceL %~ filterCards)
      & (setAsideCardsL %~ filterCards)
      & (victoryDisplayL %~ filterCards)
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
  SetActDeckCards n cards -> do
    pure $ a & (actStackL . at n ?~ cards)
  SetActDeck -> do
    let ks = sortOn Down $ a ^. actStackL . to IntMap.keys
    for_ ks $ \k -> do
      case a ^. actStackL . at k of
        Just (x : _) -> push (AddAct k x)
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
    card <- genPlayerCard cardDef
    push $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [PlayerCard card]
    if standalone
      then pure $ a & storyCardsL %~ insertWith
        (<>)
        iid
        [card { pcOwner = Just iid }]
      else pure a
  LookAtTopOfDeck iid EncounterDeckTarget n -> do
    let cards = map EncounterCard . take n $ unDeck scenarioEncounterDeck
    a <$ pushAll
      [FocusCards cards, chooseOne iid [Label "Continue" [UnfocusCards]]]
  MoveTopOfDeckToBottom _ Deck.EncounterDeck n -> do
    let (cards, deck) = draw n scenarioEncounterDeck
    pure $ a & encounterDeckL .~ withDeck (<> cards) deck
  Discard _ (TreacheryTarget tid) -> do
    card <- field TreacheryCard tid
    case card of
      PlayerCard _ -> pure a
      EncounterCard ec -> pure $ a & discardL %~ (ec :)
      VengeanceCard _ -> error "vengeance card"
  InvestigatorDoDrawEncounterCard iid -> case unDeck scenarioEncounterDeck of
    [] -> do
      when (notNull scenarioDiscard) $ do
        pushAll
          [ShuffleEncounterDiscardBackIn, InvestigatorDrawEncounterCard iid]
      pure a
      -- This case should not happen but this safeguards against it
    (card : encounterDeck) -> do
      when (null encounterDeck) $ do
        windows' <- checkWindows
          [Window Timing.When Window.EncounterDeckRunsOutOfCards]
        pushAll [windows', ShuffleEncounterDiscardBackIn]
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
          Zone.FromBottomOfDeck n -> insertWith
            (<>)
            Zone.FromDeck
            (map EncounterCard . take n . reverse $ unDeck scenarioEncounterDeck)
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
        push $ if null choices
          then chooseOne iid [Label "No cards found" []]
          else chooseN iid (min n (length choices)) choices
      DrawFoundUpTo who n -> do
        let
          choices =
            [ TargetLabel
                (CardIdTarget $ toCardId card)
                [InvestigatorDrewEncounterCard who card]
            | card <- mapMaybe (preview _EncounterCard) targetCards
            ]
        push $ if null choices
          then chooseOne iid [Label "No cards found" []]
          else chooseUpToN iid n "Do not draw more cards" choices
      DeferSearchedToTarget searchTarget -> do
        push $ if null targetCards
          then chooseOne
            iid
            [Label "No cards found" [SearchNoneFound iid searchTarget]]
          else SearchFound iid searchTarget Deck.EncounterDeck targetCards
      PlayFound{} -> error "PlayFound is not a valid EncounterDeck strategy"
      PlayFoundNoCost{} -> error "PlayFound is not a valid EncounterDeck strategy"
      ReturnCards -> pure ()

    push (FoundCards foundCards)

    pure $ a & (encounterDeckL .~ Deck encounterDeck)
  Discarded (AssetTarget _) _ (EncounterCard ec) ->
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
  DiscardEncounterUntilFirst source miid matcher -> do
    let
      (discards, remainingDeck) =
        break (`cardMatch` matcher) (unDeck scenarioEncounterDeck)
    case remainingDeck of
      [] -> do
        push (RequestedEncounterCard source miid Nothing)
        encounterDeck <- shuffleM (discards <> scenarioDiscard)
        pure $ a & encounterDeckL .~ Deck encounterDeck & discardL .~ mempty
      (x : xs) -> do
        push (RequestedEncounterCard source miid (Just x))
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
  FindEncounterCard iid target zones matcher -> do
    let
      matchingDiscards = if Zone.FromEncounterDiscard `elem` zones
        then filter (`cardMatch` matcher) scenarioDiscard
        else []
      matchingDeckCards = if Zone.FromEncounterDeck `elem` zones
        then filter (`cardMatch` matcher) (unDeck scenarioEncounterDeck)
        else []
      matchingVictoryDisplay =
        if any (`elem` zones) [Zone.FromVictoryDisplay, Zone.FromOutOfPlayAreas]
          then mapMaybe (preview _EncounterCard)
            $ filter (`cardMatch` matcher) scenarioVictoryDisplay
          else []
    matchingVoidEnemies <- if Zone.FromVoid `elem` zones
      then selectList Matcher.AnyVoidEnemy
      else pure []

    voidEnemiesWithCards <- forToSnd matchingVoidEnemies (field VoidEnemyCard)

    when
        (notNull matchingDiscards
        || notNull matchingDeckCards
        || notNull voidEnemiesWithCards
        || notNull matchingVictoryDisplay
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
      <> [ TargetLabel
             (CardIdTarget $ toCardId card)
             [FoundEncounterCardFrom iid target FromVictoryDisplay card]
         | card <- matchingVictoryDisplay
         ]
      <> [ TargetLabel
             (CardIdTarget $ toCardId card)
             [FoundEnemyInVoid iid target eid]
         | (eid, card) <- voidEnemiesWithCards
         ]

    -- TODO: show where focused cards are from

    push
      $ FocusCards
      $ map EncounterCard matchingDeckCards
      <> map EncounterCard matchingDiscards
      <> map snd voidEnemiesWithCards
    pure a
  FindAndDrawEncounterCard iid matcher includeDiscard -> do
    let
      matchingDiscards = filter (`cardMatch` matcher) scenarioDiscard
      matchingDeckCards =
        filter (`cardMatch` matcher) (unDeck scenarioEncounterDeck)

    push
      $ chooseOne iid
      $ [ TargetLabel
            (CardIdTarget $ toCardId card)
            [FoundAndDrewEncounterCard iid FromDiscard card]
        | includeDiscard
        , card <- matchingDiscards
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
    let (cards, encounterDeck) = draw n scenarioEncounterDeck
    push (RequestedEncounterCards target cards)
    pure $ a & encounterDeckL .~ encounterDeck
  DiscardTopOfEncounterDeck iid n source mtarget -> a <$ push
    (DiscardTopOfEncounterDeckWithDiscardedCards iid n source mtarget [])
  DiscardTopOfEncounterDeckWithDiscardedCards iid 0 source mtarget cards -> do
    windows' <- checkWindows
      [Window Timing.When Window.EncounterDeckRunsOutOfCards]
    pushAll
      $ [ DiscardedTopOfEncounterDeck iid cards source target
        | target <- maybeToList mtarget
        ]
      <> (guard (null scenarioEncounterDeck)
         *> [windows', ShuffleEncounterDiscardBackIn]
         )
    pure a
  DiscardTopOfEncounterDeckWithDiscardedCards iid n source mtarget discardedCards
    -> case unDeck scenarioEncounterDeck of
      [] -> do
        push $ DiscardTopOfEncounterDeckWithDiscardedCards
          iid
          0
          source
          mtarget
          discardedCards
        pure a
      (card : cards) -> do
        pushAll
          [ Discarded (CardIdTarget $ toCardId card) source (EncounterCard card)
          , DiscardTopOfEncounterDeckWithDiscardedCards
            iid
            (n - 1)
            source
            mtarget
            (card : discardedCards)
          ]
        pure $ a & discardL %~ (card :) & encounterDeckL .~ Deck cards
  SpawnEnemyAt card@(EncounterCard ec) _ -> do
    pure $ a & discardL %~ filter (/= ec) & setAsideCardsL %~ filter (/= card)
  SpawnEnemyAtEngagedWith (EncounterCard ec) _ _ -> do
    pure $ a & discardL %~ filter (/= ec)
  InvestigatorDrewEncounterCard _ ec -> do
    pure $ a & discardL %~ filter (/= ec)
  When (EnemySpawn _ _ enemyId) -> do
    card <- field EnemyCard enemyId
    pure $ a & (victoryDisplayL %~ delete card)
  SetEncounterDeck encounterDeck -> pure $ a & encounterDeckL .~ encounterDeck
  RemoveAllCopiesOfCardFromGame _ cCode ->
    pure $ a & setAsideCardsL %~ filter ((/= cCode) . toCardCode)
  Record key -> do
    isStandalone <- getIsStandalone
    pure $ if isStandalone
      then a & standaloneCampaignLogL . recordedL %~ insertSet key
      else a
  RecordCount key n -> do
    isStandalone <- getIsStandalone
    pure $ if isStandalone
      then a & standaloneCampaignLogL . recordedCountsL %~ insertMap key n
      else a
  ShuffleDeck (Deck.ScenarioDeckByKey deckKey) -> do
    deck' <- shuffleM $ fromMaybe [] (view (decksL . at deckKey) a)
    pure $ a & decksL . at deckKey ?~ deck'
  ShuffleCardsIntoDeck (Deck.ScenarioDeckByKey deckKey) cards -> do
    deck' <- shuffleM $ cards <> fromMaybe [] (filter (`notElem` cards) <$> view (decksL . at deckKey) a)
    pure $ a & decksL . at deckKey ?~ deck' & discardL %~ filter
      ((`notElem` cards) . EncounterCard)
  RemoveLocation lid -> do
    investigatorIds <-
      selectList $ Matcher.InvestigatorAt $ Matcher.LocationWithId lid
    windowMsgs <- for investigatorIds $ \iid ->
      checkWindows
        $ (`Window` Window.InvestigatorWouldBeDefeated
            (LocationSource lid)
            DefeatedByOther
            iid
          )
        <$> [Timing.When]
    pushAll $ windowMsgs <> [RemovedLocation lid]
    pure a
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
  SetupInvestigators -> do
    iids <- allInvestigatorIds
    pushAll $ map SetupInvestigator iids
    pure a
  SetScenarioMeta v -> do
    pure $ a & metaL .~ v
  RemoveCompletedActFromGame n actId -> do
    let
      completedActStack = fromMaybe mempty $ lookup n scenarioCompletedActStack
    (oldAct, actStack') <- case lookup n scenarioActStack of
      Just xs -> do
        let mFromAct = find ((== actId) . ActId . toCardCode) xs
        case mFromAct of
          Nothing -> error "Could not remove act"
          Just fromAct -> do
            push $ RemoveFromGame (ActTarget actId)
            pure (fromAct, filter (/= fromAct) xs)
      _ -> error "Can not advance act deck"
    pure
      $ a
      & (actStackL . at n ?~ actStack')
      & (completedActStackL . at n ?~ (oldAct : completedActStack))
  RestartScenario -> do
    pure $ a & (inResolutionL .~ False)
  _ -> pure a
