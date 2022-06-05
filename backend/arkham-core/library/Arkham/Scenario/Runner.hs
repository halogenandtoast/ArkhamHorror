module Arkham.Scenario.Runner where

import Arkham.Prelude

import Arkham.Act.Sequence
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Classes
import Arkham.Decks
import Arkham.Helpers
import Arkham.Helpers.Query
import Arkham.Helpers.Window
import Arkham.Id
import Arkham.Matcher qualified as Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Phase
import Arkham.Resolution
import Arkham.Scenario.Attrs
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

instance RunMessage ScenarioAttrs where
  runMessage msg a@ScenarioAttrs {..} = case msg of
    Setup -> a <$ pushEnd (Begin InvestigationPhase)
    StartCampaign -> do
      standalone <- getIsStandalone
      a <$ when standalone (push $ StartScenario scenarioName scenarioId)
    InitDeck iid deck -> do
      standalone <- getIsStandalone
      a <$ when
        standalone
        do
          (deck', randomWeaknesses) <- addRandomBasicWeaknessIfNeeded deck
          weaknesses <- traverse genPlayerCard randomWeaknesses
          push $ LoadDeck iid (Deck $ unDeck deck' <> weaknesses)
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
    Remember logKey -> pure $ a & logL %~ insertSet logKey
    ResolveToken _drawnToken token _iid | token == AutoFail ->
      a <$ push FailSkillTest
    EndOfScenario mNextCampaignStep -> do
      clearQueue
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
    ShuffleScenarioDeckIntoEncounterDeck key ->
      case lookup key scenarioDecks of
        Just [] -> pure a
        Just xs -> do
          push $ ShuffleIntoEncounterDeck $ mapMaybe (preview _EncounterCard) xs
          pure $ a & decksL %~ deleteMap key
        _ ->
          error
            $ "Invalid scenario deck key "
            <> show key
            <> ", could not find deck in scenario"
    AddCardToScenarioDeck key card -> case lookup key scenarioDecks of
      Just cards -> pure $ a & (decksL . at key ?~ card : cards)
      _ ->
        error
          $ "Invalid scenario deck key "
          <> show key
          <> ", could not find deck in scenario"
    ChooseRandomLocation target exclusions -> do
      locationIds <- setToList . (`difference` exclusions) <$> select Matcher.Anywhere
      case nonEmpty locationIds of
        Nothing -> error "no locations?"
        Just lids -> do
          randomLocationId <- sample lids
          msgs <- windows [Window.ChosenRandomLocation randomLocationId]
          a <$ pushAll (msgs <> [ChosenRandomLocation target randomLocationId])
    PlaceLocation card -> pure $ a & setAsideCardsL %~ delete card
    AddToEncounterDeck card -> do
      pure $ a & setAsideCardsL %~ deleteFirstMatch (== EncounterCard card)
    CreateStoryAssetAt card _ -> do
      pure $ a & setAsideCardsL %~ deleteFirstMatch (== card)
    AttachStoryTreacheryTo card _ -> do
      pure $ a & setAsideCardsL %~ deleteFirstMatch (== card)
    CreateEnemyAt card _ _ -> do
      pure $ a & setAsideCardsL %~ deleteFirstMatch (== card)
    PlaceUnderneath AgendaDeckTarget cards -> do
      pure $ a & cardsUnderneathAgendaDeckL <>~ cards
    PlaceUnderneath ActDeckTarget cards -> do
      for_ cards $ \card -> pushAll =<< splitWithWindows
        (PlacedUnderneath ActDeckTarget card)
        [Window.PlaceUnderneath ActDeckTarget card]
      pure a
    PlacedUnderneath ActDeckTarget card -> do
      pure $ a & cardsUnderneathActDeckL %~ (card :)
    PlaceNextTo ActDeckTarget cards -> do
      pure $ a & cardsNextToActDeckL <>~ cards
    ShuffleIntoEncounterDeck encounterCards -> do
      let
        cards = map EncounterCard encounterCards
        filterCards = filter (`notElem` cards)
      pure
        $ a
        & (cardsUnderneathAgendaDeckL %~ filterCards)
        & (cardsUnderneathActDeckL %~ filterCards)
        & (cardsNextToActDeckL %~ filterCards)
        & (cardsUnderScenarioReferenceL %~ filterCards)
        & (setAsideCardsL %~ filterCards)
    RequestSetAsideCard target cardCode -> do
      let
        (before, rest) =
          break ((== cardCode) . toCardCode) scenarioSetAsideCards
      case rest of
        [] -> error "requested a card that is not set aside"
        (x : xs) -> do
          push (RequestedSetAsideCard target x)
          pure $ a & setAsideCardsL .~ (before <> xs)
    TakeControlOfSetAsideAsset _ card -> do
      let
        cardCode = toCardCode card
        (before, rest) =
          break ((== cardCode) . toCardCode) scenarioSetAsideCards
      case rest of
        [] -> pure a
        (_ : xs) -> pure $ a & setAsideCardsL .~ (before <> xs)
    ReadStory card -> do
      leadInvestigatorId <- getLeadInvestigatorId
      push
        (chooseOne
          leadInvestigatorId
          [CardLabel (toCardCode card) [ResolveStory card]]
        )
      pure $ a & cardsUnderScenarioReferenceL %~ filter (/= card)
    SetActDeck -> do
      case a ^. actStackL . at 1 of
        Just (x : _) -> push (AddAct x)
        _ -> pure ()
      pure a
    SetAgendaDeck -> do
      case a ^. agendaStackL . at 1 of
        Just (x : _) -> push (AddAgenda x)
        _ -> pure ()
      pure a
    AddCampaignCardToDeck iid cardDef -> do
      standalone <- getIsStandalone
      if standalone
        then do
          card <- lookupPlayerCard cardDef <$> getRandom
          push (ShuffleCardsIntoDeck iid [card])
          pure $ a & storyCardsL %~ insertWith
            (<>)
            iid
            [card { pcBearer = Just iid }]
        else pure a
    _ -> pure a
