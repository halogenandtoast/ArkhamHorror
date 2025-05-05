module Arkham.Campaigns.TheForgottenAge.Helpers where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Meta
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query
import Arkham.Deck
import Arkham.Draw.Types
import Arkham.Helpers.Card
import Arkham.Helpers.Message ()
import Arkham.Helpers.Query (getInvestigators)
import Arkham.Helpers.Scenario (getVictoryDisplay, scenarioField, scenarioFieldMap)
import Arkham.History
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types
import Arkham.Location.Types
import Arkham.Matcher
import Arkham.Message (Message (..), ReplaceStrategy (..), pattern CancelNext)
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Message.Type
import Arkham.Modifier (ModifierType (..))
import Arkham.Prelude
import Arkham.Projection
import Arkham.Question (Question (..), UI (..))
import Arkham.Scenario.Deck
import Arkham.Scenario.Types
import Arkham.Source
import Arkham.Text (Tooltip (..))
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window (Result (..), mkAfter)
import Arkham.Window qualified as Window

getHasSupply :: HasGame m => InvestigatorId -> Supply -> m Bool
getHasSupply iid s = (> 0) <$> getSupplyCount iid s

getSupplyCount :: HasGame m => InvestigatorId -> Supply -> m Int
getSupplyCount iid s = fieldMap InvestigatorSupplies (length . filter (== s)) iid

getAnyHasSupply :: HasGame m => Supply -> m Bool
getAnyHasSupply = fmap notNull . getInvestigatorsWithSupply

getInvestigatorsWithSupply :: HasGame m => Supply -> m [InvestigatorId]
getInvestigatorsWithSupply s = getInvestigators >>= filterM (`getHasSupply` s)

getInvestigatorsWithoutSupply :: HasGame m => Supply -> m [InvestigatorId]
getInvestigatorsWithoutSupply s = getInvestigators >>= filterM (fmap not . (`getHasSupply` s))

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

setExplorationDeck :: (ReverseQueue m) => [Card] -> m ()
setExplorationDeck = setScenarioDeck ExplorationDeck

getSetAsidePoisonedCount :: HasGame m => m Int
getSetAsidePoisonedCount = do
  n <- selectCount $ InDeckOf Anyone <> basic (cardIs Treacheries.poisoned)
  pure $ 4 - n

getIsPoisoned :: HasGame m => InvestigatorId -> m Bool
getIsPoisoned iid = selectAny $ treacheryIs Treacheries.poisoned <> treacheryInThreatAreaOf iid

unlessPoisoned :: HasGame m => InvestigatorId -> m () -> m ()
unlessPoisoned iid body = do
  ok <- not <$> getIsPoisoned iid
  when ok body

whenPoisoned :: HasGame m => InvestigatorId -> m () -> m ()
whenPoisoned iid body = do
  ok <- getIsPoisoned iid
  when ok body

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

explore :: ReverseQueue m => InvestigatorId -> Source -> CardMatcher -> ExploreRule -> Int -> m ()
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
  case matched of
    [] -> do
      deck' <- shuffle (drawn <> rest)
      focusCards drawn do
        chooseOneM iid do
          labeled "No Matches Found" do
            unfocusCards
            setScenarioDeck ExplorationDeck deck'
    [x] -> do
      deck' <- if null notMatched then pure rest else shuffle $ rest <> notMatched
      focusCards (notMatched <> [x]) do
        chooseTargetM iid [x] \_ -> do
          unfocusCards
          setScenarioDeck ExplorationDeck deck'
      if cdCardType (toCardDef x) == LocationType
        then do
          -- let historyItem = HistoryItem HistorySuccessfulExplore True

          lid <- case exploreRule of
            PlaceExplored -> placeLocation x
            ReplaceExplored -> do
              let lSymbol = fromJustNote "no location symbol" $ cdLocationRevealedSymbol (toCardDef x)
              lid <- selectJust (LocationWithSymbol lSymbol)
              push $ ReplaceLocation lid x DefaultReplace
              pure lid

          -- we want to have kept track of revealed and without clues
          replacedIsRevealed <- field LocationRevealed lid
          replacedIsWithoutClues <- lid <=~> LocationWithoutClues

          when (canMove && exploreRule == PlaceExplored) $ moveTo source iid lid
          updateHistory iid $ HistoryItem HistorySuccessfulExplore True
          checkAfter $ Window.Explored iid (Success lid)
          when (exploreRule == ReplaceExplored) do
            setGlobal lid "replacedIsRevealed" replacedIsRevealed
            setGlobal lid "replacedIsWithoutClues" replacedIsWithoutClues
        else do
          push
            $ DrewCards iid
            $ CardDrew
              { cardDrewSource = source
              , cardDrewDeck = ScenarioDeckByKey ExplorationDeck
              , cardDrewCards = [x]
              , cardDrewAction = False
              , cardDrewRules = mempty
              , cardDrewTarget = Nothing
              }
          checkAfter $ Window.Explored iid (Failure x)
    xs -> do
      deck' <- if null notMatched then pure rest else shuffle $ rest <> notMatched
      focusCards drawn do
        chooseNM iid (min matchCount $ length xs) do
          targets xs \_ -> do
            unfocusCards
            setScenarioDeck ExplorationDeck deck'

      locations <- traverse placeLocation xs
      when canMove do
        chooseTargetM iid locations $ moveTo source iid
        updateHistory iid $ HistoryItem HistorySuccessfulExplore True

        checkWindows
          [ mkAfter $ Window.Explored iid (Success lid)
          | lid <- locations
          ]

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

pickSupplies :: ReverseQueue m => InvestigatorId -> Metadata -> [Supply] -> Message -> m ()
pickSupplies iid metadata supplies cont = do
  let remaining = findWithDefault 0 iid (supplyPoints metadata)
  when (remaining > 0) do
    player <- getPlayer iid
    investigatorSupplies <- field InvestigatorSupplies iid
    let
      availableSupply s = s `notElem` investigatorSupplies || s `elem` [Provisions, Medicine, Gasoline]
      affordableSupplies = filter ((<= remaining) . supplyCost) supplies
      availableSupplies = filter availableSupply affordableSupplies
    push
      $ Ask player
      $ PickSupplies remaining investigatorSupplies
      $ Label "Done" []
      : map (\s -> supplyLabel s [PickSupply iid s, cont]) availableSupplies

supplyLabel :: Supply -> [Message] -> UI Message
supplyLabel s = case s of
  Provisions ->
    go
      "Provisions"
      "(1 supply point each): Food and water for one person. A must-have for any journey."
  Medicine ->
    go
      "Medicine"
      "(2 supply points each): To stave off disease, infection, or venom."
  Gasoline ->
    go "Gasoline" "(1 supply points each): Enough for a long journey by car."
  Rope ->
    go
      "Rope"
      "(3 supply points): Several long coils of strong rope.  Vital for climbing and spelunking."
  Blanket -> go "Blanket" "(2 supply points): For warmth at night."
  Canteen ->
    go "Canteen" "(2 supply points): Can be refilled at streams and rivers."
  Torches ->
    go
      "Torches"
      "(3 supply points): Can light up dark areas, or set sconces alight."
  Compass ->
    go
      "Compass"
      "(2 supply points): Can guide you when you are hopelessly lost."
  Map ->
    go
      "Map"
      "(3 supply points): Unmarked for now, but with time, you may be able to map out your surroundings."
  Binoculars ->
    go "Binoculars" "(2 supply points): To help you see faraway places."
  Chalk -> go "Chalk" "(2 supply points): For writing on rough stone surfaces."
  Pendant ->
    go
      "Pendant"
      "(1 supply point): Useless, but fond memories bring comfort to travelers far from home."
  Pocketknife ->
    go
      "Pocketknife"
      "(2 supply point): Too small to be used as a reliable weapon, but easily concealed."
  Pickaxe ->
    go "Pickaxe" "(2 supply point): For breaking apart rocky surfaces."
 where
  go label tooltip = TooltipLabel label (Tooltip tooltip)

useSupply :: ReverseQueue m => InvestigatorId -> Supply -> m ()
useSupply iid s = push $ UseSupply iid s
