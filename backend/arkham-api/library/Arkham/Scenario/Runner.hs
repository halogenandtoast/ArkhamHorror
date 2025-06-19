{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Scenario.Runner (runScenarioAttrs, module X) where

import Arkham.Helpers.Message as X hiding (EnemyDamage, InvestigatorDamage)
import Arkham.Scenario.Types as X
import Arkham.Source as X
import Arkham.Target as X

import Arkham.Act.Sequence qualified as Act
import Arkham.Act.Types (Field (..))
import Arkham.Agenda.AdvancementReason
import Arkham.Agenda.Sequence qualified as Agenda
import Arkham.Agenda.Types (Field (..))
import Arkham.Asset.Types (Field (..))
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Card.PlayerCard (setPlayerCardOwner)
import Arkham.Card.Settings
import Arkham.ChaosBag ()
import Arkham.ChaosToken
import Arkham.Choose
import Arkham.Classes.Entity
import Arkham.Classes.GameLogger
import Arkham.Classes.HasChaosTokenValue
import Arkham.Classes.Query hiding (matches)
import Arkham.Classes.RunMessage
import Arkham.Collection
import Arkham.Deck qualified as Deck
import Arkham.DefeatedBy
import Arkham.Direction
import Arkham.Draw.Types
import Arkham.EncounterCard.Source
import Arkham.Enemy.Creation
import Arkham.Enemy.Types (Enemy, Field (..), enemyHealth)
import Arkham.Event.Types (Field (..))
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Helpers.Calculation
import Arkham.Helpers.Card
import Arkham.Helpers.Deck
import Arkham.Helpers.Investigator
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Helpers.Window
import Arkham.History
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Label (mkLabel)
import Arkham.Location.Grid
import Arkham.Location.Types (Field (..))
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted (fetchCard)
import Arkham.Message.Lifted.Choose
import Arkham.Name
import Arkham.Phase
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Resolution
import Arkham.Search
import Arkham.Skill.Types qualified as Field
import Arkham.Story.Types (Field (..))
import Arkham.Tarot
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Treachery.Types (Field (..))
import Arkham.Window (mkAfter, mkWhen, mkWindow)
import Arkham.Window qualified as Window
import Arkham.Zone (Zone)
import Arkham.Zone qualified as Zone
import Control.Lens (each, non, over, _1, _2)
import Data.Data.Lens (biplate)
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map

instance HasChaosTokenValue ScenarioAttrs where
  getChaosTokenValue iid chaosTokenFace _ = case chaosTokenFace of
    ElderSign -> getChaosTokenValue iid ElderSign iid
    AutoFail -> pure $ ChaosTokenValue AutoFail AutoFailModifier
    PlusOne -> pure $ ChaosTokenValue PlusOne (PositiveModifier 1)
    Zero -> pure $ ChaosTokenValue Zero (PositiveModifier 0)
    MinusOne -> pure $ ChaosTokenValue MinusOne (NegativeModifier 1)
    MinusTwo -> pure $ ChaosTokenValue MinusTwo (NegativeModifier 2)
    MinusThree -> pure $ ChaosTokenValue MinusThree (NegativeModifier 3)
    MinusFour -> pure $ ChaosTokenValue MinusFour (NegativeModifier 4)
    MinusFive -> pure $ ChaosTokenValue MinusFive (NegativeModifier 5)
    MinusSix -> pure $ ChaosTokenValue MinusSix (NegativeModifier 6)
    MinusSeven -> pure $ ChaosTokenValue MinusSeven (NegativeModifier 7)
    MinusEight -> pure $ ChaosTokenValue MinusEight (NegativeModifier 8)
    otherFace -> pure $ ChaosTokenValue otherFace NoModifier

instance RunMessage ScenarioAttrs where
  runMessage msg a =
    runScenarioAttrs msg a >>= traverseOf chaosBagL (runMessage msg)

runScenarioAttrs :: Runner ScenarioAttrs
runScenarioAttrs msg a@ScenarioAttrs {..} = runQueueT $ case msg of
  ResetGame -> do
    whenM getIsStandalone do
      for_ (mapToList scenarioPlayerDecks) $ \(iid, deck) -> do
        let deckCardCodes = map toCardCode $ unDeck deck
        let ifShouldAdd pc = pc.cardCode `notElem` deckCardCodes
        let investigatorStoryCards = filter ifShouldAdd $ findWithDefault [] iid scenarioStoryCards
        push $ LoadDeck iid (Deck $ unDeck deck <> investigatorStoryCards)
    pure $ overAttrs (inResolutionL .~ False) a
  BeginGame -> do
    mFalseAwakeningPointOfNoReturn <-
      getMaybeCampaignStoryCard Treacheries.falseAwakeningPointOfNoReturn
    for_ mFalseAwakeningPointOfNoReturn \falseAwakening -> do
      tid <- getRandom
      pushAll
        [ AttachStoryTreacheryTo tid (toCard falseAwakening) AgendaDeckTarget
        , PlaceDoom (toSource tid) (toTarget tid) 1
        ]

    mFalseAwakening <- getMaybeCampaignStoryCard Treacheries.falseAwakening
    for_ mFalseAwakening \falseAwakening -> do
      tid <- getRandom
      pushAll
        [ AttachStoryTreacheryTo tid (toCard falseAwakening) AgendaDeckTarget
        , PlaceDoom (toSource tid) (toTarget tid) 1
        ]

    pure a
  BeginRound -> do
    push $ Do BeginRound
    pure $ a & turnL +~ 1
  StartCampaign -> do
    standalone <- getIsStandalone
    when standalone $ do
      players <- allPlayers
      lead <- getActivePlayer
      pushAll
        $ [Ask lead PickScenarioSettings | not scenarioIsSideStory]
        <> [ chooseDecks players
           , ResetInvestigators
           , ResetGame
           , StartScenario scenarioId
           ]
    pure a
  InitDeck iid _ deck -> do
    standalone <- getIsStandalone
    if standalone
      then do
        investigatorClass <- field InvestigatorClass iid
        playerCount <- getPlayerCount
        let cardCodes = map toCardCode $ unDeck deck

        mEldritchBrand <-
          if "11080" `elem` cardCodes
            then
              getMaybeCardAttachments iid (CardCode "11080") >>= \case
                Nothing -> do
                  pid <- getPlayer iid
                  let cards = nub $ map toCardCode $ filterCards (card_ $ #asset <> #spell) (unDeck deck)
                  pure $ Just $ Ask pid $ QuestionLabel "Choose card for Eldritch Brand (5)" Nothing $ ChooseOne $ flip map cards \c ->
                    CardLabel c [UpdateCardSetting iid "11080" (SetCardSetting CardAttachments [c])]
                Just _ -> pure Nothing
            else pure Nothing
        (deck', randomWeaknesses) <- addRandomBasicWeaknessIfNeeded investigatorClass playerCount deck
        weaknesses <- traverse (`genPlayerCardWith` setPlayerCardOwner iid) randomWeaknesses
        purchaseTrauma <- initDeckTrauma deck' iid (toTarget a)
        initXp <- initDeckXp deck' iid (toTarget a)
        let deck'' = withDeck (<> weaknesses) deck'

        pushAll
          $ LoadDeck iid deck''
          : purchaseTrauma
            <> toList mEldritchBrand
            <> [DoStep 1 msg]
            <> initXp
        pure $ a & playerDecksL %~ insertMap iid deck''
      else pure a
  DoStep 1 (InitDeck iid _ deck) -> do
    standalone <- getIsStandalone
    when standalone do
      let cardCodes = map toCardCode $ unDeck deck
      mSpiritualHealing <-
        if "11098" `elem` cardCodes
          then do
            mentalTrauma <- field InvestigatorMentalTrauma iid
            physicalTrauma <- field InvestigatorPhysicalTrauma iid
            pid <- getPlayer iid
            pure
              $ if
                | mentalTrauma > 0 && physicalTrauma > 0 ->
                    Just
                      $ chooseOne
                        pid
                        [ Label "Heal 1 Physical Trauma" [HealTrauma iid 1 0]
                        , Label "Heal 1 Mental Trauma" [HealTrauma iid 0 1]
                        ]
                | physicalTrauma > 0 -> Just $ HealTrauma iid 1 0
                | mentalTrauma > 0 -> Just $ HealTrauma iid 0 1
                | otherwise -> Nothing
          else pure Nothing
      for_ mSpiritualHealing push
    pure a
  EndSetup -> do
    pushAll [BeginGame, BeginRound, Begin InvestigationPhase]
    pure a
  ResolveAmounts iid choiceMap (LabeledTarget "Purchase Trauma" (isTarget a -> True)) -> do
    let physical = getChoiceAmount "$physical" choiceMap
    let mental = getChoiceAmount "$mental" choiceMap
    push $ SufferTrauma iid physical mental
    pure a
  PlaceLocationMatching cardMatcher -> do
    let
      matches =
        filter
          (`cardMatch` (Matcher.CardWithType LocationType <> cardMatcher))
          scenarioSetAsideCards
    locationId <- getRandom
    a <$ case matches of
      [] -> error "There were no locations with that name"
      (card : _) -> push (PlaceLocation locationId card)
  PlaceDoomOnAgenda n canAdvance -> do
    agendaIds <- select Matcher.AnyAgenda
    case agendaIds of
      [] -> pure ()
      [x] -> push (PlaceTokens (toSource a) (AgendaTarget x) Doom n)
      _ -> error "multiple agendas should be handled by the scenario"
    pushWhen (canAdvance == CanAdvance) AdvanceAgendaIfThresholdSatisfied
    pure a
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
          go (y : ys) as =
            if cdStage (toCardDef y) /= Just n
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
    let completedActStack = fromMaybe mempty $ lookup n scenarioCompletedActStack
    (oldAct, actStack') <- case lookup n scenarioActStack of
      Just (x : y : ys) -> do
        let fromActId = ActId $ toCardCode x
        push $ ReplaceAct fromActId y
        pure (x, y : ys)
      _ -> error "Can not advance act deck"
    pure
      $ a
      & actStackL
      . at n
      ?~ actStack'
      & (completedActStackL . at n ?~ (oldAct : completedActStack))
  SetCurrentActDeck n stack@(current : _) -> do
    actIds <- select $ Matcher.ActWithDeckId n
    pushAll
      $ [toDiscard GameSource (ActTarget actId) | actId <- actIds]
      <> [AddAct n current]
    pure
      $ a
      & (actStackL . at n ?~ stack)
      & (setAsideCardsL %~ filter (`notElem` stack))
  SetCurrentAgendaDeck n stack@(current : _) -> do
    agendaIds <- select $ Matcher.AgendaWithDeckId n
    pushAll
      $ [toDiscard GameSource (AgendaTarget agendaId) | agendaId <- agendaIds]
      <> [AddAgenda n current]
    pure
      $ a
      & (agendaStackL . at n ?~ stack)
      & (setAsideCardsL %~ filter (`notElem` stack))
  AdvanceToAct n actDef newActSide _ -> do
    let completedActStack = fromMaybe mempty $ lookup n scenarioCompletedActStack
    (oldAct, actStack') <- case lookup n scenarioActStack of
      Just (x : ys) -> do
        let fromActId = ActId (toCardCode x)
        case find (`isCard` actDef) ys of
          Nothing -> do
            -- Todo we add this here because of how the search for kadath moves
            -- through acts and this seems better than failing. However maybe
            -- we want to handle this in the scenario directly or come up with
            -- some way not to remove acts at the same level
            toAct <- genCard actDef
            let toActId = ActId (toCardCode toAct)
            push $ ReplaceAct fromActId toAct
            pushWhen (newActSide == Act.B)
              $ AdvanceAct toActId (toSource a) AdvancedWithOther
            pure
              ( x
              , toAct
                  : filter
                    ( \c ->
                        (cdStage (toCardDef c) /= cdStage actDef)
                          || (toCardCode c `cardCodeExactEq` toCardCode actDef)
                    )
                    ys
              )
          Just toAct -> do
            let toActId = ActId (toCardCode toAct)
            push $ ReplaceAct fromActId toAct
            pushWhen (newActSide == Act.B)
              $ AdvanceAct toActId (toSource a) AdvancedWithOther
            pure
              ( x
              , filter
                  ( \c ->
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
          go (y : ys) as =
            if cdStage (toCardDef y) /= Just n
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
  Do (AdvanceToAgenda n agendaDef newAgendaSide _) -> do
    agendaStack' <- case lookup n scenarioAgendaStack of
      Just (x : ys) -> do
        let fromAgendaId = AgendaId (toCardCode x)
        case find (`isCard` agendaDef) ys of
          Nothing -> do
            card <- fetchCard agendaDef
            let toAgendaId = AgendaId (toCardCode card)
            push (ReplaceAgenda fromAgendaId card)
            when (newAgendaSide == Agenda.B) $ push $ AdvanceAgendaBy toAgendaId #other
            pure
              $ card
              : filter
                ( \c ->
                    fromMaybe
                      False
                      (liftA2 (>) (cdStage $ toCardDef c) (cdStage agendaDef))
                      || (toCardCode c `cardCodeExactEq` toCardCode agendaDef)
                )
                ys
          Just toAgenda -> do
            let toAgendaId = AgendaId (toCardCode toAgenda)
            push (ReplaceAgenda fromAgendaId toAgenda)
            when (newAgendaSide == Agenda.B) $ push $ AdvanceAgendaBy toAgendaId #other
            -- filter the stack so only agendas with higher stages are left
            pure
              $ filter
                ( \c ->
                    fromMaybe
                      False
                      (liftA2 (>) (cdStage $ toCardDef c) (cdStage agendaDef))
                      || (toCardCode c `cardCodeExactEq` toCardCode agendaDef)
                )
                ys
      _ -> error "Can not advance agenda deck"
    pure $ a & agendaStackL . at n ?~ agendaStack'
  Discard _ _ (ActTarget _) -> pure $ a & actStackL .~ mempty
  -- See: Vengeance Awaits / The Devourer Below - right now the assumption
  -- is that the act deck has been replaced.
  CheckForRemainingInvestigators -> do
    investigatorIds <- select Matcher.UneliminatedInvestigator
    when (null investigatorIds && not scenarioInResolution) $ do
      push $ HandleNoRemainingInvestigators scenarioNoRemainingInvestigatorsHandler
    pure a
  AllInvestigatorsResigned -> do
    push $ HandleNoRemainingInvestigators scenarioNoRemainingInvestigatorsHandler
    pure a
  SetNoRemainingInvestigatorsHandler target -> do
    pure $ a & noRemainingInvestigatorsHandlerL .~ target
  HandleNoRemainingInvestigators target | isTarget a target -> do
    clearQueue
    push (ScenarioResolution NoResolution)
    pure $ a & inResolutionL .~ True -- must set to avoid redundancy when scenario kills investigator
  InvestigatorWhenEliminated _ iid mmsg -> do
    whenMsg <- checkWindows [mkWhen (Window.InvestigatorEliminated iid)]
    afterMsg <- checkWindows [mkAfter (Window.InvestigatorEliminated iid)]
    pushAll
      $ [ whenMsg
        , InvestigatorEliminated iid
        ]
      <> maybeToList mmsg
      <> [ After (InvestigatorEliminated iid)
         , afterMsg
         , CheckForRemainingInvestigators
         ]
    pure a
  Remember logKey -> do
    send $ "Remember \"" <> format logKey <> "\""
    pure $ a & logL %~ insertSet logKey
  Forget logKey -> do
    send $ "Forgot \"" <> format logKey <> "\""
    pure $ a & logL %~ deleteSet logKey
  ScenarioCountSet logKey n -> do
    pushM $ checkWindows [mkAfter $ Window.ScenarioCountIncremented logKey]
    pure $ a & countsL %~ Map.alter (const (Just n)) logKey
  ScenarioCountIncrementBy logKey n -> do
    pushM $ checkWindows [mkAfter $ Window.ScenarioCountIncremented logKey]
    pure $ a & countsL %~ Map.alter (Just . maybe n (+ n)) logKey
  ScenarioCountDecrementBy logKey n ->
    pure
      $ a
      & countsL
      %~ Map.alter (Just . max 0 . maybe 0 (subtract n)) logKey
  ResolveChaosToken drawnToken token iid -> do
    shouldResolve <-
      withoutModifiers (ChaosTokenTarget drawnToken) [IgnoreChaosTokenEffects, IgnoreChaosToken]
    when shouldResolve do
      ChaosTokenValue _ tokenModifier <- getChaosTokenValue iid token ()
      if tokenModifier == AutoFailModifier
        then push FailSkillTest
        else do
          when (token `elem` [#curse, #bless, #frost]) do
            shouldRevealAnother <- withoutModifier (ChaosTokenTarget drawnToken) DoNotRevealAnotherChaosToken
            pushWhen shouldRevealAnother (DrawAnotherChaosToken iid)
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
  ChooseFrom iid choose | Just key <- collectionToScenarioDeckKey choose.collection ->
    case lookup key scenarioDecks of
      Just [] -> pure a
      Just xs -> do
        (drew, rest) <- splitAt choose.amount <$> shuffleM xs
        push $ ChoseCards iid $ finalizeChoose choose drew
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
  SetScenarioDeck key [] -> pure $ a & (decksL %~ deleteMap key)
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
    locationIds <- (\\ exclusions) <$> select Matcher.Anywhere
    case nonEmpty locationIds of
      Nothing -> error "no locations?"
      Just lids -> do
        randomLocationId <- sample lids
        pushAll
          $ windows [Window.ChosenRandomLocation randomLocationId]
          <> [ChosenRandomLocation target randomLocationId]
    pure a
  SetCardAside card -> pure $ a & setAsideCardsL %~ (card :)
  ReplaceAgenda _ card -> pure $ a & setAsideCardsL %~ delete card
  PlaceLocation _ card -> pure $ a & setAsideCardsL %~ delete card & decksL . each %~ delete card
  ReplaceLocation _ card _ -> pure $ a & setAsideCardsL %~ delete card
  CreateWeaknessInThreatArea card _ -> pure $ a & setAsideCardsL %~ delete card
  ShuffleCardsIntoTopOfDeck Deck.EncounterDeck n (onlyEncounterCards -> cards) -> do
    let (cards', rest) = draw n scenarioEncounterDeck
    shuffled <- shuffleM (cards <> cards')
    pure $ a & encounterDeckL .~ Deck (shuffled <> unDeck rest)
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
      let encounterDeck = withDeck ((<> [ec]) . deleteFirst ec) scenarioEncounterDeck
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
  AddToEncounterDiscard ec -> do
    handler <- getEncounterDeckHandler (toCardId ec)
    pure
      $ a
      & (discardLens handler %~ (ec :))
      & (encounterDeckL %~ withDeck (filter (/= ec)))
      & (victoryDisplayL %~ filter (/= EncounterCard ec))
      & (setAsideCardsL %~ filter (/= EncounterCard ec))
  AddToVictory (SkillTarget sid) -> do
    card <- field Field.SkillCard sid
    pure $ a & (victoryDisplayL %~ (card :))
  AddToVictory (EventTarget eid) -> do
    card <- field EventCard eid
    pure $ a & (victoryDisplayL %~ (card :))
  AddToVictory (StoryTarget eid) -> do
    card <- field StoryCard eid
    pure $ a & (victoryDisplayL %~ nub . (card :))
  AddToVictory (AssetTarget tid) -> do
    card <- field AssetCard tid
    pure $ a & (victoryDisplayL %~ nub . (card :))
  AddToVictory (TreacheryTarget tid) -> do
    card <- field TreacheryCard tid
    pure $ a & (victoryDisplayL %~ nub . (card :))
  AddToVictory (ActTarget aid) -> do
    flipped <- field ActFlipped aid
    card <- field ActCard aid
    let card' = if flipped then flipCard card else card
    pure $ a & (victoryDisplayL %~ nub . (card' :))
  AddToVictory (AgendaTarget aid) -> do
    card <- field AgendaCard aid
    pure $ a & (victoryDisplayL %~ nub . (card :))
  RemoveEnemy eid -> do
    fieldMay EnemyPlacement eid >>= \case
      Just (OutOfPlay Zone.VictoryDisplayZone) -> do
        mcard <- fieldMay EnemyCard eid
        pure $ case mcard of
          Just card -> a & (victoryDisplayL %~ nub . (card :))
          Nothing -> a
      _ -> pure a
  AddToVictory (LocationTarget lid) -> do
    card <- field LocationCard lid
    pure $ a & (victoryDisplayL %~ nub . (card :))
  AddToVictory (EnemyTarget eid) -> do
    card <- field EnemyCard eid
    pure $ a & (victoryDisplayL %~ nub . (card :))
  DefeatedAddToVictory (EnemyTarget eid) -> do
    card <- field EnemyCard eid
    pure $ a & (victoryDisplayL %~ nub . (card :))
  AddToVictory (CardIdTarget cid) -> do
    card <- getCard cid
    selectOne (Matcher.EnemyWithCardId cid) >>= \case
      Nothing -> pure $ a & (victoryDisplayL %~ nub . (card :))
      Just _ -> pure a
  Discarded (EnemyTarget eid) _ _ -> do
    card <- convertToCard eid
    placement <- field EnemyPlacement eid
    case placement of
      AsSwarm {} -> pure a
      _ -> do
        handler <- getEncounterDeckHandler $ toCardId card
        case card of
          PlayerCard _ -> pure a
          EncounterCard ec -> pure $ a & discardLens handler %~ (ec :)
          VengeanceCard _ -> error "vengeance card"
  Discarded (LocationTarget lid) _ _ -> do
    card <- convertToCard lid
    handler <- getEncounterDeckHandler $ toCardId card
    -- only single sided encounter cards should end up in discard
    case card of
      PlayerCard _ -> pure a
      EncounterCard ec ->
        if cdDoubleSided (toCardDef card)
          then pure a
          else pure $ a & discardLens handler %~ (ec :)
      VengeanceCard _ -> error "vengeance card"
  CreateAssetAt _ card _ -> do
    pure $ a & setAsideCardsL %~ deleteFirstMatch (== card)
  AttachStoryTreacheryTo _ card _ -> do
    pure $ a & setAsideCardsL %~ deleteFirstMatch (== card)
  CreateEnemy (enemyCreationCard -> card) -> do
    pure
      $ a
      & (setAsideCardsL %~ deleteFirstMatch (== card))
      & (victoryDisplayL %~ filter (/= card))
      & (encounterDeckL %~ withDeck (filter ((/= card) . EncounterCard)))
      & (discardL %~ filter ((/= card) . EncounterCard))
  PlaceUnderneath AgendaDeckTarget cards -> do
    for_ cards $ \card ->
      pushAll
        $ ObtainCard card.id
        : splitWithWindows
          (PlacedUnderneath AgendaDeckTarget card)
          [Window.PlaceUnderneath AgendaDeckTarget card]
    pure $ a & setAsideCardsL %~ filter (`notElem` cards)
  PlaceUnderneath ActDeckTarget cards -> do
    for_ cards $ \card ->
      pushAll
        $ ObtainCard card.id
        : splitWithWindows
          (PlacedUnderneath ActDeckTarget card)
          [Window.PlaceUnderneath ActDeckTarget card]
    pure $ a & setAsideCardsL %~ filter (`notElem` cards)
  PlaceUnderneath ScenarioTarget cards -> do
    pure $ a & cardsUnderScenarioReferenceL <>~ cards & setAsideCardsL %~ filter (`notElem` cards)
  PlaceUnderneath _ cards -> do
    pure $ a & setAsideCardsL %~ filter (`notElem` cards)
  CardEnteredPlay _ card -> liftRunMessage (ObtainCard card.id) a
  ObtainCard cardId -> do
    let
      deleteCard :: IsCard c => [c] -> [c]
      deleteCard = deleteFirstMatch ((== cardId) . toCardId)
    pure
      $ a
      & (setAsideCardsL %~ deleteCard)
      & (encounterDeckL %~ withDeck deleteCard)
      & (victoryDisplayL %~ deleteCard)
      & (discardL %~ deleteCard)
      & (cardsUnderScenarioReferenceL %~ deleteCard)
      & (cardsUnderAgendaDeckL %~ deleteCard)
      & (cardsUnderActDeckL %~ deleteCard)
      & (cardsNextToActDeckL %~ deleteCard)
      & (cardsNextToAgendaDeckL %~ deleteCard)
      & (decksL . each %~ deleteCard)
      & (encounterDecksL . each %~ bimap (withDeck deleteCard) deleteCard)
  PlacedUnderneath ActDeckTarget card -> do
    pure $ a & cardsUnderActDeckL %~ (card :)
  PlacedUnderneath AgendaDeckTarget card -> do
    pure $ a & cardsUnderAgendaDeckL %~ (card :)
  PlaceNextTo ActDeckTarget cards -> do
    pure $ a & cardsNextToActDeckL <>~ cards
  PlaceNextTo AgendaDeckTarget cards -> do
    pure $ a & cardsNextToAgendaDeckL <>~ cards
  ShuffleCardsIntoDeck Deck.EncounterDeck cards -> do
    push $ ShuffleCardsIntoDeck (Deck.EncounterDeckByKey RegularEncounterDeck) cards
    pure a
  ShuffleCardsIntoDeck (Deck.EncounterDeckByKey _) [] -> do
    pure a
  ShuffleCardsIntoDeck (Deck.EncounterDeckByKey deckKey) cards -> do
    let
      encounterCards = mapMaybe (preview _EncounterCard) cards
      filterOutCards = filter (`notElem` cards)
    deck' <-
      withDeckM
        (shuffleM . (<> encounterCards) . filter (`notElem` encounterCards))
        (a ^. encounterDeckLensFromKey deckKey)
    pure
      $ a
      & (cardsUnderAgendaDeckL %~ filterOutCards)
      & (cardsUnderActDeckL %~ filterOutCards)
      & (cardsNextToActDeckL %~ filterOutCards)
      & (cardsNextToAgendaDeckL %~ filterOutCards)
      & (cardsUnderScenarioReferenceL %~ filterOutCards)
      & (setAsideCardsL %~ filterOutCards)
      & (victoryDisplayL %~ filterOutCards)
      & (encounterDeckLensFromKey deckKey .~ deck')
  ShuffleCardsIntoDeck (Deck.ScenarioDeckByKey deckKey) cards -> do
    let filterOutCards = filter (`notElem` cards)
    let encounterCards = mapMaybe (preview _EncounterCard) cards
    deck' <- shuffleM $ cards <> maybe [] filterOutCards (view (decksL . at deckKey) a)
    pure
      $ a
      & (encounterDeckL %~ filter (`notElem` encounterCards))
      & (encounterDecksL . each . _2 %~ filter (`notElem` encounterCards))
      & (encounterDecksL . each . _1 %~ withDeck (filter (`notElem` encounterCards)))
      & (decksL . at deckKey ?~ deck')
      & (discardL %~ filter ((`notElem` cards) . EncounterCard))
      & (victoryDisplayL %~ filterOutCards)
      & (setAsideCardsL %~ filterOutCards)
  ShuffleCardsIntoDeck _ cards -> do
    let
      encounterCards = mapMaybe (preview _EncounterCard) cards
      filterOutCards = filter (`notElem` cards)
    pure
      $ a
      & (cardsUnderAgendaDeckL %~ filterOutCards)
      & (cardsUnderActDeckL %~ filterOutCards)
      & (cardsNextToActDeckL %~ filterOutCards)
      & (cardsNextToAgendaDeckL %~ filterOutCards)
      & (cardsUnderScenarioReferenceL %~ filterOutCards)
      & (setAsideCardsL %~ filterOutCards)
      & (victoryDisplayL %~ filterOutCards)
      & (encounterDeckL %~ filter (`notElem` encounterCards))
      & (encounterDecksL . each . _2 %~ filter (`notElem` encounterCards))
      & (encounterDecksL . each . _1 %~ withDeck (filter (`notElem` encounterCards)))
      & (decksL . each %~ filterOutCards)
  TakeControlOfSetAsideAsset _ card -> do
    let
      cardCode = toCardCode card
      (before, rest) = break ((== cardCode) . toCardCode) scenarioSetAsideCards
    case rest of
      [] -> pure a
      (_ : xs) -> pure $ a & setAsideCardsL .~ (before <> xs)
  ReadStory _ card _ _ -> do
    pure $ a & cardsUnderScenarioReferenceL %~ filter (/= card)
  ResolveStory _ ResolveIt storyId -> do
    pure $ a & resolvedStoriesL %~ (storyId :)
  SetActDeckCards n cards -> do
    pure $ a & actStackL %~ insertMap n cards
  SetAgendaDeckCards n cards -> do
    pure $ a & agendaStackL %~ insertMap n cards
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
  AddCampaignCardToDeck iid shouldShuffleIn card -> do
    standalone <- getIsStandalone
    let card' = overPlayerCard (setPlayerCardOwner iid) card
    replaceCard card.id card'
    when (shouldShuffleIn == ShuffleIn) do
      push $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [card']
    pure $ if standalone then a & storyCardsL %~ insertWith (<>) iid (onlyPlayerCards [card']) else a
  LookAtTopOfDeck iid EncounterDeckTarget n -> do
    let cards = map EncounterCard . take n $ unDeck scenarioEncounterDeck
    player <- getPlayer iid
    pushAll [FocusCards cards, chooseOne player [Label "Continue" [UnfocusCards]]]
    pure a
  MoveTopOfDeckToBottom _ Deck.EncounterDeck n -> do
    let (cards, deck) = draw n scenarioEncounterDeck
    pure $ a & encounterDeckL .~ withDeck (<> cards) deck
  Discarded (TreacheryTarget _) _ card -> do
    case card of
      PlayerCard _ -> pure a
      EncounterCard ec -> do
        handler <- getEncounterDeckHandler $ toCardId card
        pure $ a & discardLens handler %~ (ec :)
      VengeanceCard _ -> error "vengeance card"
  DrewCards iid drew | isNothing drew.target -> do
    let playerCards = onlyPlayerCards drew.cards
    when (notNull playerCards) do
      pushAll $ InvestigatorDrewPlayerCardFrom iid <$> playerCards <*> pure (Just drew.deck)

    let encounterCards = onlyEncounterCards drew.cards
    when (notNull encounterCards) do
      pushAll $ InvestigatorDrewEncounterCardFrom iid <$> encounterCards <*> pure (Just drew.deck)

    pure a
  Do (DrawCards iid drawing) | Just key <- Deck.deckSignifierToScenarioDeckKey drawing.deck -> do
    case lookup key scenarioDecks of
      Just [] -> pure a
      Just xs -> do
        let (drew, rest) = splitAt drawing.amount xs
        push $ DrewCards iid $ finalizeDraw drawing drew
        pure $ a & decksL . at key ?~ rest
      _ ->
        error
          $ "Invalid scenario deck key "
          <> show key
          <> ", could not find deck in scenario"
  Do (DrawCards iid drawing) | drawing.deck == Deck.EncounterDeck -> do
    handler <- getEncounterDeckHandler iid
    key <- getEncounterDeckKey iid
    case unDeck (a ^. deckLens handler) of
      [] -> do
        when (notNull (a ^. discardLens handler)) $ do
          pushAll [ShuffleEncounterDiscardBackInByKey key, Do (DrawCards iid drawing)]
        pure a
      xs -> do
        let (drew, rest) = splitAt drawing.amount xs
        if length drew == drawing.amount
          then do
            when (null rest && not scenarioInShuffle) do
              windows' <- checkWindows [mkWhen Window.EncounterDeckRunsOutOfCards]
              pushAll [windows', ShuffleEncounterDiscardBackInByKey key]
            push $ DrewCards iid $ finalizeDraw drawing $ drawing.alreadyDrawn <> map toCard drew
          else do
            when (null rest && not scenarioInShuffle) do
              windows' <- checkWindows [mkWhen Window.EncounterDeckRunsOutOfCards]
              pushAll [windows', ShuffleEncounterDiscardBackInByKey key]

            push
              $ Do
              $ DrawCards iid
              $ drawing {cardDrawAlreadyDrawn = map toCard drew, cardDrawAmount = drawing.amount - length drew}
        pure $ a & (deckLens handler .~ Deck rest) & (inShuffleL .~ null rest)
  Search (MkSearch searchType iid _ EncounterDeckTarget _ _ _ _ _) -> do
    case searchType of
      Searching ->
        wouldDo
          msg
          (Window.WouldSearchDeck iid Deck.EncounterDeck)
          (Window.SearchedDeck iid Deck.EncounterDeck)
      Looking ->
        wouldDo
          msg
          (Window.WouldLookAtDeck iid Deck.EncounterDeck)
          (Window.LookedAtDeck iid Deck.EncounterDeck)
      Revealing -> do
        batchId <- getRandom
        push $ DoBatch batchId msg
    pure a
  DoBatch
    batchId
    (Search (MkSearch sType iid source EncounterDeckTarget cardSources cardMatcher foundStrategy _ _)) -> do
      mods <- getModifiers iid
      let
        additionalDepth =
          sum [x | sType == Searching, SearchDepth x <- mods]
            + sum [x | sType == Looking, LookAtDepth x <- mods]
        foundCards :: Map Zone [Card] =
          foldl'
            ( \hmap (cardSource, _) -> case cardSource of
                Zone.FromDeck ->
                  insertWith
                    (<>)
                    Zone.FromDeck
                    (map EncounterCard $ unDeck scenarioEncounterDeck)
                    hmap
                Zone.FromTopOfDeck n ->
                  insertWith
                    (<>)
                    Zone.FromDeck
                    (map EncounterCard . take (n + additionalDepth) $ unDeck scenarioEncounterDeck)
                    hmap
                Zone.FromBottomOfDeck n ->
                  insertWith
                    (<>)
                    Zone.FromDeck
                    (map EncounterCard . take (n + additionalDepth) . reverse $ unDeck scenarioEncounterDeck)
                    hmap
                Zone.FromDiscard ->
                  insertWith
                    (<>)
                    Zone.FromDiscard
                    (map EncounterCard scenarioDiscard)
                    hmap
                other -> error $ mconcat ["Zone ", show other, " not yet handled"]
            )
            mempty
            cardSources
      targetCards <- filterM (`extendedCardMatch` cardMatcher) $ concat $ toList foundCards
      player <- getPlayer iid

      pushBatch batchId (FoundCards foundCards)

      let
        applyMod (AdditionalTargets n) = over biplate (+ n)
        applyMod _ = id
        foundStrategy' = foldr applyMod foundStrategy mods

      case foundStrategy' of
        DrawOrCommitFound {} -> error "CommitFound not implemented for EncounterDeck"
        AddToHandOrPlayFound {} -> error "AddToHandOrPlayFound not implemented for EncounterDeck"
        RemoveFoundFromGame _ _ -> error "Unhandled"
        DrawFound who n -> do
          let
            choices =
              [ targetLabel (toCardId card) [InvestigatorDrewEncounterCard who card]
              | EncounterCard card <- targetCards
              ]
          pushBatch batchId
            $ if null choices
              then chooseOne player [Label "No cards found" []]
              else chooseN player (min n (length choices)) choices
        DrawFoundUpTo who n -> do
          let
            choices =
              [ targetLabel (toCardId card) [InvestigatorDrewEncounterCard who card]
              | EncounterCard card <- targetCards
              ]
          pushBatch batchId
            $ if null choices
              then chooseOne player [Label "No cards found" []]
              else chooseUpToN player n "Do not draw more cards" choices
        DeferSearchedToTarget searchTarget _ -> do
          pushBatch batchId
            $ if null targetCards
              then
                chooseOne
                  player
                  [Label "No cards found" [SearchNoneFound iid searchTarget]]
              else SearchFound iid searchTarget Deck.EncounterDeck targetCards
        DrawAllFound who -> do
          let
            choices =
              [ targetLabel (toCardId card) [InvestigatorDrewEncounterCard who card]
              | EncounterCard card <- targetCards
              ]
          pushBatch batchId
            $ if null choices
              then chooseOne player [Label "No cards found" []]
              else chooseOneAtATime player choices
        PlayFound {} -> error "PlayFound is not a valid EncounterDeck strategy"
        PlayFoundNoCost {} -> error "PlayFound is not a valid EncounterDeck strategy"
        ReturnCards -> pure ()

      pushBatch batchId $ EndSearch iid source EncounterDeckTarget cardSources
      pure a
  Discarded (AssetTarget _) _ card@(EncounterCard ec) -> do
    handler <- getEncounterDeckHandler $ toCardId card
    -- TODO: determine why this was only specified for Asset
    pure $ a & discardLens handler %~ (ec :)
  ResignWith (AssetTarget aid) -> do
    cardCode <- field AssetCardCode aid
    pure $ a & resignedCardCodesL %~ (cardCode :)
  RemoveFromEncounterDiscard ec -> pure $ a & discardL %~ filter (/= ec)
  RemoveFromEncounterDeck ec -> pure $ a & encounterDeckL %~ filter (/= ec)
  ShuffleDeck Deck.EncounterDeck -> do
    encounterDeck <- withDeckM shuffleM scenarioEncounterDeck
    pure $ a & encounterDeckL .~ encounterDeck
  ShuffleEncounterDiscardBackInByKey key -> do
    case key of
      RegularEncounterDeck -> do
        push ShuffleEncounterDiscardBackIn
        pure a
      other -> do
        let otherDiscardL = encounterDecksL . at other . non (Deck [], []) . _2
        let discards = a ^. otherDiscardL
        encounterDeck <- withDeckM (shuffleM . (<> discards)) (view (encounterDeckLensFromKey other) a)
        pure $ a & encounterDecksL . at other . non (Deck [], []) .~ (encounterDeck, mempty)
  ShuffleEncounterDiscardBackIn -> do
    encounterDeck <- withDeckM (shuffleM . (<> scenarioDiscard)) scenarioEncounterDeck
    pure $ a & encounterDeckL .~ encounterDeck & discardL .~ mempty
  ShuffleAllInEncounterDiscardBackIn cardCode -> do
    let (toShuffleBackIn, discard) = partition ((== cardCode) . toCardCode) scenarioDiscard
    encounterDeck <- withDeckM (shuffleM . (<> toShuffleBackIn)) scenarioEncounterDeck
    pure $ a & encounterDeckL .~ encounterDeck & discardL .~ discard
  ShuffleBackIntoEncounterDeck (EnemyTarget eid) -> do
    placement <- field EnemyPlacement eid
    card <- case placement of
      AsSwarm _ c -> pure c
      _ -> field EnemyCard eid
    push $ RemoveEnemy eid

    case card of
      EncounterCard card' -> do
        encounterDeck <- withDeckM (shuffleM . (card' :)) scenarioEncounterDeck
        pure $ a & encounterDeckL .~ encounterDeck
      PlayerCard card' -> do
        case toCardOwner card' of
          Just iid -> do
            push $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [card]
            pure a
          Nothing -> error "must be owned"
      _ -> error "must be encounter card"
  ShuffleBackIntoEncounterDeck (LocationTarget lid) -> do
    card <- field LocationCard lid
    case card of
      EncounterCard card' -> do
        pushAll $ resolve (RemoveLocation lid)
        encounterDeck <- withDeckM (shuffleM . (card' :)) scenarioEncounterDeck
        pure $ a & encounterDeckL .~ encounterDeck
      _ -> error "must be encounter card"
  DiscardUntilFirst iid source Deck.EncounterDeck matcher -> do
    push $ DiscardUntilFirst iid source (Deck.EncounterDeckByKey RegularEncounterDeck) matcher
    pure a
  DiscardUntilFirst iid source (Deck.EncounterDeckByKey RegularEncounterDeck) matcher -> do
    (discards, remainingDeck) <- breakM (`extendedCardMatch` matcher) (unDeck scenarioEncounterDeck)
    case remainingDeck of
      [] -> do
        push (RequestedEncounterCard source (Just iid) Nothing)
        encounterDeck <- shuffleM (discards <> scenarioDiscard)
        pure $ a & encounterDeckL .~ Deck encounterDeck & discardL .~ mempty
      (x : xs) -> do
        push (RequestedEncounterCard source (Just iid) (Just x))
        pure $ a & encounterDeckL .~ Deck xs & discardL %~ (reverse discards <>)
  DiscardUntilFirst iid source (Deck.EncounterDeckByKey k) matcher -> do
    (discards, remainingDeck) <-
      breakM (`extendedCardMatch` matcher) (unDeck $ a ^. encounterDeckLensFromKey k)
    case remainingDeck of
      [] -> do
        let otherDiscardL = encounterDecksL . at k . non (Deck [], []) . _2
        let currentDiscard = a ^. otherDiscardL
        push $ RequestedEncounterCard source (Just iid) Nothing
        encounterDeck <- shuffle (discards <> currentDiscard)
        pure $ a & encounterDecksL . at k . non (Deck [], []) .~ (Deck encounterDeck, mempty)
      (x : xs) -> do
        push $ RequestedEncounterCard source (Just iid) (Just x)
        -- pure $ a & encounterDeckL .~ Deck xs & discardL %~ (reverse discards <>)
        pure $ a & encounterDecksL . at k . non (Deck [], []) %~ (\(_, zs) -> (Deck xs, x : zs))
  DiscardUntilN n iid source target Deck.EncounterDeck matcher -> do
    push $ DiscardUntilN n iid source target (Deck.EncounterDeckByKey RegularEncounterDeck) matcher
    pure a
  DiscardUntilN n _ _ target (Deck.EncounterDeckByKey RegularEncounterDeck) matcher -> do
    (discards, remainingDeck) <- breakNM n (`extendedCardMatch` matcher) (unDeck scenarioEncounterDeck)
    matches <- filterM (`extendedCardMatch` matcher) discards
    case remainingDeck of
      [] -> do
        push (RequestedEncounterCards target matches)
        encounterDeck <- shuffleM (discards <> scenarioDiscard)
        pure $ a & encounterDeckL .~ Deck encounterDeck & discardL .~ mempty
      xs -> do
        push (RequestedEncounterCards target matches)
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
      encounterDecksF = case cardSource of
        FromEncounterDeck -> encounterDecksL . each . _1 %~ withDeck (filter ((/= cardId) . toCardId))
        _ -> id
    shuffled <- shuffleM encounterDeck
    push (InvestigatorDrewEncounterCard iid card)
    pure $ a & (encounterDeckL .~ Deck shuffled) & (discardL .~ discard) & encounterDecksF
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
      matchingDiscards =
        if Zone.FromEncounterDiscard `elem` zones
          then filter (`cardMatch` matcher) scenarioDiscard
          else []
      matchingDeckCards =
        if Zone.FromEncounterDeck `elem` zones
          then filter (`cardMatch` matcher) (unDeck scenarioEncounterDeck)
          else []
      matchingVictoryDisplay =
        if Zone.FromOutOfPlayArea Zone.VictoryDisplayZone `elem` zones
          then
            mapMaybe (preview _EncounterCard)
              $ filter (`cardMatch` matcher) scenarioVictoryDisplay
          else []
    matchingVoidEnemies <-
      if Zone.FromOutOfPlayArea Zone.VoidZone `elem` zones
        then select $ Matcher.OutOfPlayEnemy Zone.VoidZone Matcher.AnyEnemy
        else pure []

    voidEnemiesWithCards <-
      forToSnd
        matchingVoidEnemies
        (field @(OutOfPlayEntity 'Zone.VoidZone Enemy) (OutOfPlayEnemyField Zone.VoidZone EnemyCard))

    player <- getPlayer iid

    -- TODO: show where focused cards are from

    push
      $ FocusCards
      $ map EncounterCard matchingDeckCards
      <> map EncounterCard matchingDiscards
      <> map snd voidEnemiesWithCards

    when
      ( notNull matchingDiscards
          || notNull matchingDeckCards
          || notNull voidEnemiesWithCards
          || notNull matchingVictoryDisplay
      )
      $ push
      $ chooseOne player
      $ [ targetLabel card [FoundEncounterCardFrom iid target FromDiscard card, UnfocusCards]
        | card <- matchingDiscards
        ]
      <> [ targetLabel card [FoundEncounterCardFrom iid target FromEncounterDeck card, UnfocusCards]
         | card <- matchingDeckCards
         ]
      <> [ targetLabel card [FoundEncounterCardFrom iid target FromVictoryDisplay card, UnfocusCards]
         | card <- matchingVictoryDisplay
         ]
      <> [ targetLabel card [FoundEnemyInOutOfPlay Zone.VoidZone iid target eid, UnfocusCards]
         | (eid, card) <- voidEnemiesWithCards
         ]

    pure a
  FindAndDrawEncounterCard iid matcher includeDiscard -> do
    handler <- getEncounterDeckHandler iid
    let
      matchingDiscards = filter (`cardMatch` matcher) (a ^. discardLens handler)
      matchingDeckCards =
        filter (`cardMatch` matcher) (unDeck $ a ^. deckLens handler)

    player <- getPlayer iid

    let
      matches =
        [ targetLabel
            (toCardId card)
            [FoundAndDrewEncounterCard iid FromDiscard card]
        | includeDiscard == IncludeDiscard
        , card <- matchingDiscards
        ]
          <> [ targetLabel
                 (toCardId card)
                 [FoundAndDrewEncounterCard iid FromEncounterDeck card]
             | card <- matchingDeckCards
             ]

    if null matches
      then push $ chooseOne player [Label "No matches found" []]
      else do
        -- TODO: show where focused cards are from
        push
          $ FocusCards
          $ map EncounterCard matchingDeckCards
          <> map EncounterCard matchingDiscards
        push $ chooseOne player matches
    pure a
  DrawEncounterCards target n -> do
    let (cards, encounterDeck) = draw n scenarioEncounterDeck
    push (RequestedEncounterCards target cards)
    pure $ a & encounterDeckL .~ encounterDeck
  DiscardTopOfEncounterDeck iid n source mtarget -> do
    push $ DiscardTopOfEncounterDeckWithDiscardedCards iid n source mtarget []
    pure a
  DiscardTopOfEncounterDeckWithDiscardedCards iid 0 source mtarget cards -> do
    windows' <- checkWindows [mkWhen Window.EncounterDeckRunsOutOfCards]
    pushAll
      $ ( guard (null scenarioEncounterDeck && not scenarioInShuffle)
            *> [windows', ShuffleEncounterDiscardBackIn]
        )
      <> [ DiscardedTopOfEncounterDeck iid cards source target
         | target <- maybeToList mtarget
         ]
    pure $ a & inShuffleL .~ null scenarioEncounterDeck
  DiscardTopOfEncounterDeckWithDiscardedCards iid n source mtarget discardedCards -> do
    handler <- getEncounterDeckHandler iid
    case unDeck (a ^. deckLens handler) of
      [] -> do
        push $ DiscardTopOfEncounterDeckWithDiscardedCards iid 0 source mtarget discardedCards
        pure a
      (card : cards) -> do
        beforeWindow <- checkWindows [mkWhen (Window.Discarded (Just iid) source (EncounterCard card))]
        afterWindow <- checkWindows [mkAfter (Window.Discarded (Just iid) source (EncounterCard card))]
        pushAll
          [ beforeWindow
          , Discarded (CardIdTarget $ toCardId card) source (EncounterCard card)
          , afterWindow
          , DiscardTopOfEncounterDeckWithDiscardedCards iid (n - 1) source mtarget (card : discardedCards)
          ]
        pure $ a & deckLens handler .~ Deck cards & discardLens handler %~ (card :)
  SpawnEnemyAt card@(EncounterCard ec) _ -> do
    pure $ a & discardL %~ filter (/= ec) & setAsideCardsL %~ filter (/= card)
  AddToHand _ cards -> do
    pure $ a & setAsideCardsL %~ filter (`notElem` cards)
  SpawnEnemyAtEngagedWith (EncounterCard ec) _ _ -> do
    pure $ a & discardL %~ filter (/= ec)
  InvestigatorDrewEncounterCard _ ec -> do
    pure
      $ a
      & (discardL %~ filter (/= ec))
      & (encounterDeckL %~ withDeck (filter (/= ec)))
      & (decksL . each %~ filter (/= toCard ec))
  InvestigatorDrewEncounterCardFrom _ ec _ -> do
    pure
      $ a
      & (discardL %~ filter (/= ec))
      & (encounterDeckL %~ withDeck (filter (/= ec)))
      & (decksL . each %~ filter (/= toCard ec))
  When (EnemySpawn ((.enemy) -> enemyId)) -> do
    card <- field EnemyCard enemyId
    pure $ a & (victoryDisplayL %~ delete card)
  SetEncounterDeck encounterDeck -> do
    pure $ a & encounterDeckL .~ encounterDeck & hasEncounterDeckL .~ True
  RemoveAllCopiesOfCardFromGame _ cCode ->
    pure
      $ a
      & (setAsideCardsL %~ filter ((/= cCode) . toCardCode))
      & (encounterDeckL %~ filter ((/= cCode) . toCardCode))
      & (discardL %~ filter ((/= cCode) . toCardCode))
      & (decksL . each %~ filter ((/= cCode) . toCardCode))
  RemoveAllCopiesOfEncounterCardFromGame cardMatcher ->
    pure
      $ a
      & (setAsideCardsL %~ filter (not . (`cardMatch` cardMatcher)))
      & (encounterDeckL %~ filter (not . (`cardMatch` cardMatcher)))
      & (discardL %~ filter (not . (`cardMatch` cardMatcher)))
      & (decksL . each %~ filter (not . (`cardMatch` cardMatcher)))
  SetCampaignLog newLog -> do
    isStandalone <- getIsStandalone
    pure
      $ if isStandalone
        then a & standaloneCampaignLogL .~ newLog
        else a
  Record key -> do
    isStandalone <- getIsStandalone
    pure
      $ if isStandalone
        then a & standaloneCampaignLogL . recordedL %~ insertSet key
        else a
  RecordCount key n -> do
    isStandalone <- getIsStandalone
    pure
      $ if isStandalone
        then a & standaloneCampaignLogL . recordedCountsL %~ insertMap key n
        else a
  IncrementRecordCount key n -> do
    isStandalone <- getIsStandalone
    pure
      $ if isStandalone
        then a & standaloneCampaignLogL . recordedCountsL %~ alterMap (Just . maybe n (+ n)) key
        else a
  RecordSetInsert key recs -> do
    isStandalone <- getIsStandalone
    if isStandalone
      then do
        let defs = mapMaybe lookupCardDef $ recordedCardCodes recs
        for_ defs $ \def ->
          send $ "Record \"" <> format (toName def) <> " " <> format key <> "\""
        pure $ case a ^. standaloneCampaignLogL . recordedSetsL . at key of
          Nothing -> a & standaloneCampaignLogL . recordedSetsL %~ insertMap key recs
          Just set ->
            let set' = filter (`notElem` recs) set <> recs
             in a & standaloneCampaignLogL . recordedSetsL %~ insertMap key set'
      else pure a
  RecordSetReplace key v v' -> do
    isStandalone <- getIsStandalone
    if isStandalone
      then do
        pure $ case a ^. standaloneCampaignLogL . recordedSetsL . at key of
          Nothing -> a & standaloneCampaignLogL . recordedSetsL %~ insertMap key (singleton v')
          Just set ->
            let set' = map (\x -> if x == v then v' else x) set
             in a & standaloneCampaignLogL . recordedSetsL %~ insertMap key set'
      else pure a
  ShuffleDeck (Deck.ScenarioDeckByKey deckKey) -> do
    deck' <- shuffleM $ fromMaybe [] (view (decksL . at deckKey) a)
    pure $ a & decksL . at deckKey ?~ deck'
  RemoveLocation lid -> do
    investigatorIds <-
      select $ Matcher.InvestigatorAt $ Matcher.LocationWithId lid

    windowMsgs <- for investigatorIds $ \iid ->
      checkWindows
        $ ( `mkWindow`
              Window.InvestigatorWouldBeDefeated
                (DefeatedByOther $ LocationSource lid)
                iid
          )
        <$> [Timing.When]
    pushAll $ windowMsgs <> [RemovedLocation lid]
    pure a
  RemoveAllDoomFromPlay matchers -> do
    let Matcher.RemoveDoomMatchers {..} = matchers
    xs <-
      fold
        <$> sequence
          [ selectMap LocationTarget (removeDoomLocations <> Matcher.LocationWithAnyDoom)
          , selectMap InvestigatorTarget removeDoomInvestigators
          , selectMap
              EnemyTarget
              (removeDoomEnemies <> Matcher.EnemyWithAnyDoom <> Matcher.EnemyWithoutModifier DoNotRemoveDoom)
          , selectMap AssetTarget (removeDoomAssets <> Matcher.AssetWithAnyDoom)
          , selectMap ActTarget removeDoomActs
          , selectMap AgendaTarget (removeDoomAgendas <> Matcher.AgendaWithAnyDoom)
          , selectMap TreacheryTarget removeDoomTreacheries
          , selectMap EventTarget removeDoomEvents
          , selectMap SkillTarget removeDoomSkills
          ]
    pushAll [RemoveAllDoom (toSource a) target | target <- xs]
    pure a
  SetupInvestigators -> do
    iids <- allInvestigators
    pushAll $ map SetupInvestigator iids <> [DrawStartingHands]
    pure a
  DrawStartingHands -> do
    iids <- allInvestigators
    for_ iids \iid -> do
      beforeDrawingStartingHand <- checkWindows [mkWhen (Window.DrawingStartingHand iid)]
      pushAll [beforeDrawingStartingHand, DrawStartingHand iid]
    pure a
  SetScenarioMeta v -> do
    pure $ a & metaL .~ v
  LoadTarotDeck -> do
    tarotDeck' <- shuffleM (toList tarotDeck)
    pure $ a & tarotDeckL .~ tarotDeck'
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
  PlaceKey ScenarioTarget k -> do
    pure $ a & setAsideKeysL %~ deleteSet k & keysL %~ insertSet k
  PlaceKey target k | not (isTarget a target) -> do
    pure $ a & (setAsideKeysL %~ deleteSet k)
  RemoveTokens _ ScenarioTarget token amount -> do
    pure $ a & tokensL %~ subtractTokens token amount
  RestartScenario -> do
    pure $ a & (inResolutionL .~ False)
  PerformReading Chaos -> do
    card <- TarotCard <$> sample2 Upright Reversed <*> sample (NE.fromList scenarioTarotDeck)
    sendTarot $ toJSON [card]
    pure
      $ a
      & (tarotCardsL . at GlobalTarot . non [] .~ [card])
      & tarotDeckL
      %~ delete (toTarotArcana card)
  PerformReading Balance -> do
    cards <- sampleN 2 (NE.fromList scenarioTarotDeck)
    case cards of
      [c1, c2] -> do
        sendTarot $ toJSON [TarotCard Upright c1, TarotCard Reversed c2]
        pure
          $ a
          & (tarotCardsL . at GlobalTarot . non [] .~ [TarotCard Upright c1, TarotCard Reversed c2])
          & tarotDeckL
          %~ filter (`notElem` cards)
      _ -> error "impossible"
  PerformReading Choice -> do
    lead <- getLeadPlayer
    cards <- map (TarotCard Upright) <$> sampleN 3 (NE.fromList scenarioTarotDeck)
    pushAll
      [ FocusTarotCards cards
      , questionLabel "Choose two cards to rotate" lead
          $ ChooseN 2 [TarotLabel card [RotateTarot card] | card <- cards]
      , UnfocusTarotCards
      ]
    pure
      $ a
      & tarotCardsL
      . at GlobalTarot
      . non []
      .~ cards
      & tarotDeckL
      %~ filter (`notElem` map toTarotArcana cards)
  DrawAndChooseTarot iid facing n -> do
    cards <- map (TarotCard facing) <$> sampleN n (NE.fromList scenarioTarotDeck)
    player <- getPlayer iid
    push $ chooseOrRunOne player [TarotLabel card [PlaceTarot iid card] | card <- cards]
    pure a
  PlaceTarot iid card -> do
    tarotDeck' <- shuffleM $ delete (toTarotArcana card) scenarioTarotDeck
    pure $ a & tarotDeckL .~ tarotDeck' & tarotCardsL . at (InvestigatorTarot iid) . non [] %~ (card :)
  RotateTarot (toTarotArcana -> arcana) -> do
    let
      rotate = \case
        TarotCard Upright arcana' | arcana' == arcana -> TarotCard Reversed arcana'
        TarotCard Reversed arcana' | arcana' == arcana -> TarotCard Upright arcana'
        c -> c
    pure $ a & tarotCardsL . each %~ map rotate
  After (X.EnemyDefeated eid _ _ _) -> do
    eattrs <- getAttrs @Enemy eid
    printedHealth <- calculatePrinted (enemyHealth eattrs)
    enemyHealth <- fieldWithDefault printedHealth EnemyHealth eid
    pure $ a & defeatedEnemiesL %~ insertMap eid (DefeatedEnemyAttrs eattrs enemyHealth)
  SetAsideCards cards -> do
    pure $ a & setAsideCardsL <>~ cards
  SetLayout layout -> do
    pure $ a & locationLayoutL .~ layout
  ChooseLeadInvestigator -> do
    iids <- getInvestigators
    case iids of
      [x] -> push $ ChoosePlayer x SetLeadInvestigator
      xs@(x : _) -> do
        player <- getPlayer x
        push
          $ questionLabel "Choose lead investigator" player
          $ ChooseOne
            [ PortraitLabel iid [ChoosePlayer iid SetLeadInvestigator]
            | iid <- xs
            ]
      [] -> pure ()
    pure a
  ReportXp breakdown -> do
    pure $ a & xpBreakdownL ?~ breakdown
  PlaceGrid gloc@(GridLocation pos lid) -> do
    let grid = insertGrid gloc scenarioGrid
    mTopLocation <-
      selectOne $ Matcher.LocationWithLabel (mkLabel $ gridLabel $ updatePosition pos GridUp)
    mBottomLocation <-
      selectOne $ Matcher.LocationWithLabel (mkLabel $ gridLabel $ updatePosition pos GridDown)
    mLeftLocation <-
      selectOne $ Matcher.LocationWithLabel (mkLabel $ gridLabel $ updatePosition pos GridLeft)
    mRightLocation <-
      selectOne $ Matcher.LocationWithLabel (mkLabel $ gridLabel $ updatePosition pos GridRight)
    pushAll
      $ [ LocationMoved lid
        , SetLocationLabel lid (gridLabel pos)
        , SetLayout (gridToTemplate grid)
        ]
      <> [PlacedLocationDirection lid Below topLocation | topLocation <- maybeToList mTopLocation]
      <> [PlacedLocationDirection lid Above bottomLocation | bottomLocation <- maybeToList mBottomLocation]
      <> [PlacedLocationDirection lid LeftOf rightLocation | rightLocation <- maybeToList mRightLocation]
      <> [PlacedLocationDirection lid RightOf leftLocation | leftLocation <- maybeToList mLeftLocation]
    pure $ a & gridL .~ grid
  ForTarget ScenarioTarget msg' -> liftRunMessage msg' a
  UseAbility _ ab _ | isSource a ab.source || isProxySource a ab.source -> do
    push $ Do msg
    pure a
  AdvanceAgendaIfThresholdSatisfied -> do
    select Matcher.AgendaWantsToAdvance >>= \case
      [x] -> do
        whenMsg <- checkWindows [mkWhen (Window.AgendaWouldAdvance DoomThreshold x)]
        afterMsg <- checkWindows [mkAfter (Window.AgendaWouldAdvance DoomThreshold x)]
        pushAll [whenMsg, afterMsg, ForTarget (toTarget x) AdvanceAgendaIfThresholdSatisfied]
      xs -> leadChooseOneM do
        targets xs \x -> do
          whenMsg <- checkWindows [mkWhen (Window.AgendaWouldAdvance DoomThreshold x)]
          afterMsg <- checkWindows [mkAfter (Window.AgendaWouldAdvance DoomThreshold x)]
          pushAll [whenMsg, afterMsg, ForTarget (toTarget x) AdvanceAgendaIfThresholdSatisfied]
    pure a
  ReadStoryWithPlacement _ card _ _ _ -> do
    pure $ a & setAsideCardsL %~ filter (/= card)
  _ -> pure a
