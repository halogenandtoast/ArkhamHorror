module Arkham.Campaigns.TheForgottenAge.Helpers where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Deck
import Arkham.Draw.Types
import Arkham.Game.Helpers
import Arkham.Helpers.Card
import Arkham.Helpers.Message
import Arkham.History
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types
import Arkham.Location.Types
import Arkham.Matcher
import Arkham.Message.Lifted.Queue
import Arkham.Movement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenario.Types
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window (Result (..), mkWindow)
import Arkham.Window qualified as Window

getHasSupply :: HasGame m => InvestigatorId -> Supply -> m Bool
getHasSupply iid s = (> 0) <$> getSupplyCount iid s

getSupplyCount :: HasGame m => InvestigatorId -> Supply -> m Int
getSupplyCount iid s =
  fieldMap InvestigatorSupplies (length . filter (== s)) iid

getAnyHasSupply :: HasGame m => Supply -> m Bool
getAnyHasSupply = fmap notNull . getInvestigatorsWithSupply

getInvestigatorsWithSupply :: HasGame m => Supply -> m [InvestigatorId]
getInvestigatorsWithSupply s =
  getInvestigators >>= filterM (`getHasSupply` s)

getInvestigatorsWithoutSupply :: HasGame m => Supply -> m [InvestigatorId]
getInvestigatorsWithoutSupply s =
  getInvestigators >>= filterM (fmap not . (`getHasSupply` s))

getVengeanceInVictoryDisplay :: forall m. (HasCallStack, HasGame m) => m Int
getVengeanceInVictoryDisplay = do
  victoryDisplay <- getVictoryDisplay
  let
    isVengeanceCard = \case
      VengeanceCard _ -> True
      _ -> False
    inVictoryDisplay' =
      sum $ map (fromMaybe 0 . cdVengeancePoints . toCardDef) victoryDisplay
    vengeanceCards = count isVengeanceCard victoryDisplay
  locationVengeance <- fmap getSum . toVengeance =<< select (RevealedLocation <> LocationWithoutClues)
  locationsWithModifier <-
    getSum
      <$> selectAgg
        (Sum . fromMaybe 0)
        LocationVengeance
        (LocationWithModifier InVictoryDisplayForCountingVengeance)
  pure $ inVictoryDisplay' + locationsWithModifier + vengeanceCards + locationVengeance
 where
  toVengeance :: ConvertToCard c => [c] -> m (Sum Int)
  toVengeance = fmap (mconcat . map Sum . catMaybes) . traverse getVengeancePoints

getExplorationDeck :: HasGame m => m [Card]
getExplorationDeck = scenarioFieldMap ScenarioDecks (findWithDefault [] ExplorationDeck)

getSetAsidePoisonedCount :: HasGame m => m Int
getSetAsidePoisonedCount = do
  n <- selectCount $ InDeckOf Anyone <> basic (cardIs Treacheries.poisoned)
  pure $ 4 - n

getIsPoisoned :: HasGame m => InvestigatorId -> m Bool
getIsPoisoned iid = selectAny $ treacheryIs Treacheries.poisoned <> treacheryInThreatAreaOf iid

getUnpoisoned :: HasGame m => m [InvestigatorId]
getUnpoisoned = select $ NotInvestigator $ HasMatchingTreachery $ treacheryIs $ Treacheries.poisoned

getSetAsidePoisoned :: HasGame m => m Card
getSetAsidePoisoned =
  fromJustNote "not enough poison cards"
    . find ((== Treacheries.poisoned) . toCardDef)
    <$> scenarioField ScenarioSetAsideCards

data ExploreRule = PlaceExplored | ReplaceExplored
  deriving stock Eq

-- ReplaceExplored should actually place the location on "top"

explore
  :: (HasQueue Message m, HasGame m, MonadRandom m)
  => InvestigatorId
  -> Source
  -> CardMatcher
  -> ExploreRule
  -> Int
  -> m ()
explore iid source cardMatcher exploreRule matchCount = do
  explorationDeck <- getExplorationDeck
  canMove <- iid <=~> InvestigatorCanMove
  let
    cardMatcher' = CardWithOneOf [CardWithType TreacheryType, cardMatcher]
    splitAtMatch d = case break (`cardMatch` cardMatcher') d of
      (l, []) -> (l, [])
      (l, x : xs) -> (l <> [x], xs)
    (drawn, rest) =
      foldr
        ( \_ (drawn', rest') ->
            let (drawn'', rest'') = splitAtMatch rest'
             in (drawn' <> drawn'', rest'')
        )
        ([], explorationDeck)
        [1 .. matchCount]
    (matched, notMatched) = partition (`cardMatch` cardMatcher') drawn
  player <- getPlayer iid
  case matched of
    [] -> do
      deck' <- shuffleM (drawn <> rest)
      pushAll
        [ FocusCards drawn
        , chooseOne
            player
            [ Label
                "No Matches Found"
                [UnfocusCards, SetScenarioDeck ExplorationDeck deck']
            ]
        ]
    [x] -> do
      msgs <-
        if cdCardType (toCardDef x) == LocationType
          then do
            let historyItem = HistoryItem HistorySuccessfulExplore True

            (lid, locationAction) <- case exploreRule of
              PlaceExplored -> placeLocation x
              ReplaceExplored -> do
                let
                  lSymbol =
                    fromJustNote "no location symbol"
                      $ cdLocationRevealedSymbol (toCardDef x)
                mLocationToReplace <- selectOne $ LocationWithSymbol lSymbol
                case mLocationToReplace of
                  Just lid -> pure (lid, ReplaceLocation lid x DefaultReplace)
                  Nothing -> error "no location found"

            afterExploredWindow <-
              checkWindows
                [mkWindow Timing.After $ Window.Explored iid (Success lid)]

            -- we want to have kept track of revealed and without clues
            replacedIsRevealed <- field LocationRevealed lid
            replacedIsWithoutClues <- lid <=~> LocationWithoutClues

            pure
              $ locationAction
              : [ Move $ move source iid lid
                | canMove && exploreRule == PlaceExplored
                ]
                <> [ UpdateHistory iid historyItem
                   , afterExploredWindow
                   ]
                <> ( guard (exploreRule == ReplaceExplored)
                      *> [ SetGlobal (toTarget lid) "replacedIsRevealed" (toJSON replacedIsRevealed)
                         , SetGlobal (toTarget lid) "replacedIsWithoutClues" (toJSON replacedIsWithoutClues)
                         ]
                   )
          else do
            windowMsg <-
              checkWindows
                [mkWindow Timing.After $ Window.Explored iid (Failure x)]
            pure
              [ DrewCards iid
                  $ CardDrew
                    { cardDrewSource = source
                    , cardDrewDeck = ScenarioDeckByKey ExplorationDeck
                    , cardDrewCards = [x]
                    , cardDrewAction = False
                    , cardDrewRules = mempty
                    , cardDrewTarget = Nothing
                    }
              , windowMsg
              ]
      deck' <-
        if null notMatched
          then pure rest
          else shuffleM (rest <> notMatched)
      pushAll
        [ FocusCards (notMatched <> [x])
        , chooseOne
            player
            [ targetLabel
                (toCardId x)
                (UnfocusCards : SetScenarioDeck ExplorationDeck deck' : msgs)
            ]
        ]
    xs -> do
      -- we assume only locations, triggered by forked path
      -- This can only be PlaceExplored
      msgs <- do
        placements <- traverse placeLocation xs
        let
          historyItem = HistoryItem HistorySuccessfulExplore True
          locationIds = map fst placements

        afterExploredWindow <-
          checkWindows
            [ mkWindow Timing.After $ Window.Explored iid (Success lid)
            | lid <- locationIds
            ]

        pure
          $ map snd placements
          <> [ chooseOne
              player
              [ targetLabel lid [Move $ move source iid lid]
              | lid <- locationIds
              ]
             | canMove
             ]
          <> [ UpdateHistory iid historyItem
             , afterExploredWindow
             ]
      deck' <-
        if null notMatched
          then pure rest
          else shuffleM (rest <> notMatched)
      -- TODO: Uh why is this effect empty here?
      pushAll
        $ [ FocusCards drawn
          , chooseN
              player
              (min matchCount $ length xs)
              [targetLabel (toCardId x) [] | x <- xs]
          , UnfocusCards
          , SetScenarioDeck ExplorationDeck deck'
          ]
        <> msgs

getVengeancePoints :: (HasCallStack, ConvertToCard c, HasGame m) => c -> m (Maybe Int)
getVengeancePoints = getCardField cdVengeancePoints

getHasVengeancePoints :: (ConvertToCard c, HasGame m) => c -> m Bool
getHasVengeancePoints c = isJust <$> getVengeancePoints c

exploreAction :: Cost -> AbilityType
exploreAction cost = ActionAbility [#explore] (ActionCost 1 <> cost)

exploreAction_ :: AbilityType
exploreAction_ = exploreAction mempty

cancelExplore :: ReverseQueue m => Sourceable source => source -> m ()
cancelExplore source = push $ CancelNext (toSource source) ExploreMessage

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "theForgottenAge" a
