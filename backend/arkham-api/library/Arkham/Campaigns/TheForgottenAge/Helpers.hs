module Arkham.Campaigns.TheForgottenAge.Helpers where

import Arkham.Ability
import Arkham.Calculation
import Arkham.Campaigns.TheForgottenAge.Key
import Arkham.Campaigns.TheForgottenAge.Meta
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query
import Arkham.Deck
import Arkham.Draw.Types
import Arkham.Enemy.CardDefs.ReturnToTheForgottenAge.ReturnToTheDoomOfEztli qualified as Enemies
import Arkham.Enemy.CardDefs.TheForgottenAge.TheDoomOfEztli qualified as Enemies
import Arkham.Enemy.Creation (EnemyCreation)
import Arkham.Helpers.Card
import Arkham.Helpers.Location (getLocationOf, toConnections)
import Arkham.Helpers.Message ()
import Arkham.Helpers.Modifiers (getModifiers)
import Arkham.Helpers.Query (getInvestigators)
import Arkham.Helpers.Scenario (getInResolution, getVictoryDisplay, scenarioField, scenarioFieldMap)
import Arkham.History
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types
import Arkham.Location.Types
import Arkham.Matcher
import Arkham.Message (
  Message (..),
  ReplaceStrategy (..),
  ShuffleIn (..),
  pattern BeginSkillTest,
  pattern CancelNext,
 )
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (incrementRecordCount)
import Arkham.Message.Lifted.Move
import Arkham.Message.Type
import Arkham.Modifier (ModifierType (..))
import Arkham.Name (toTitle)
import Arkham.Prelude
import Arkham.Projection
import Arkham.Question (Question (..), UI (..))
import Arkham.Scenario.Deck
import Arkham.Scenario.Types
import Arkham.Scenario.Types qualified as Scenario
import Arkham.SkillTest.Base
import Arkham.SkillType (SkillType)
import Arkham.Source
import Arkham.Target
import Arkham.Text (Tooltip (..))
import Arkham.Treachery.CardDefs.ReturnToTheForgottenAge.ReturnToTheDepthsOfYoth qualified as Treacheries
import Arkham.Treachery.CardDefs.TheForgottenAge.Poison qualified as Treacheries
import Arkham.Window (Result (..), mkAfter)
import Arkham.Window qualified as Window

pickSupply :: ReverseQueue m => InvestigatorId -> Supply -> m ()
pickSupply iid s = push $ PickSupply iid s

getHasSupply :: HasGame m => InvestigatorId -> Supply -> m Bool
getHasSupply iid s = (> 0) <$> getSupplyCount iid s

getSupplyCount :: HasGame m => InvestigatorId -> Supply -> m Int
getSupplyCount iid s = fieldMap InvestigatorSupplies (length . filter (== s)) iid

getAnyHasSupply :: HasGame m => Supply -> m Bool
getAnyHasSupply = fmap notNull . getInvestigatorsWithSupply

unlessAnyHasSupply :: HasGame m => Supply -> m () -> m ()
unlessAnyHasSupply s = unlessM (getAnyHasSupply s)

getInvestigatorsWithSupply :: HasGame m => Supply -> m [InvestigatorId]
getInvestigatorsWithSupply s = getInvestigators >>= filterM (`getHasSupply` s)

getInvestigatorsWithoutSupply :: HasGame m => Supply -> m [InvestigatorId]
getInvestigatorsWithoutSupply s = getInvestigators >>= filterM (fmap not . (`getHasSupply` s))

-- | The i18n key naming the vengeance tally in the campaign log breakdown.
vengeanceTally :: Text
vengeanceTally = "$upgrade.tally.vengeance"

{- | Label for a vengeance source that isn't a card, under the campaign scope so
it resolves the same from any scenario.
-}
vengeanceLabel :: Text -> Text
vengeanceLabel k = campaignI18n $ scope "vengeance" $ ikey' k

cardTallyLabel :: (ConvertToCard c, HasGame m) => c -> m Text
cardTallyLabel c = toTitle . RevealedCard <$> convertToCard c

{- | Collapse repeated labels into one row. A location can be counted both by
'InVictoryDisplayForCountingVengeance' and by the revealed/clue-free rule, and
the same card can be in the victory display twice; the total is unchanged.
-}
mergeTallyEntries :: [(Text, Int)] -> [(Text, Int)]
mergeTallyEntries entries =
  [(k, sum [n | (k', n) <- entries, k' == k]) | k <- nub (map fst entries)]

{- | Per-source attribution for the vengeance currently in the victory display.
'getVengeanceInVictoryDisplay' is the sum of these, so the campaign log can
never disagree with the recorded tally.
-}
getVengeanceEntries :: forall m. (HasCallStack, HasGame m) => m [(Text, Int)]
getVengeanceEntries = do
  victoryDisplay <- getVictoryDisplay
  cardEntries <- for victoryDisplay \card -> do
    let printed = fromMaybe 0 $ cdVengeancePoints $ toCardDef card
    let bonus = case card of
          VengeanceCard _ -> 1
          _ -> 0
    (,printed + bonus) <$> cardTallyLabel card
  locationEntries <-
    traverse toLocationEntry
      =<< select (LocationWithModifier InVictoryDisplayForCountingVengeance)
  pure $ mergeTallyEntries $ filter ((> 0) . snd) (cardEntries <> locationEntries)
 where
  toLocationEntry lid = do
    n <- fieldMap LocationVengeance (fromMaybe 0) lid
    (,n) <$> cardTallyLabel lid

{- | 'getVengeanceEntries' plus the revealed, clue-free locations that count at
the end of the scenario.
-}
getTotalVengeanceEntries :: forall m. (HasCallStack, HasGame m) => m [(Text, Int)]
getTotalVengeanceEntries = do
  entries <- getVengeanceEntries
  locations <- select (RevealedLocation <> LocationWithoutClues)
  locationEntries <- for locations \lid -> do
    n <- fromMaybe 0 <$> getVengeancePoints lid
    (,n) <$> cardTallyLabel lid
  pure $ mergeTallyEntries $ entries <> filter ((> 0) . snd) locationEntries

getTotalVengeanceInVictoryDisplay :: (HasCallStack, HasGame m) => m Int
getTotalVengeanceInVictoryDisplay = sum . map snd <$> getTotalVengeanceEntries

getVengeanceInVictoryDisplay :: (HasCallStack, HasGame m) => m Int
getVengeanceInVictoryDisplay = sum . map snd <$> getVengeanceEntries

{- | Add Yig's Fury from a source that isn't a card in the victory display, and
report it so the campaign log shows where it came from.
-}
addVengeance :: ReverseQueue m => Text -> Int -> m ()
addVengeance from n = when (n /= 0) do
  incrementRecordCount YigsFury n
  reportTally vengeanceTally from n

{- | Add the vengeance in the victory display to Yig's Fury, reporting each
contributing card so the campaign log can attribute the tally.
-}
recordVengeance :: ReverseQueue m => m ()
recordVengeance = do
  entries <- getTotalVengeanceEntries
  incrementRecordCount YigsFury (sum $ map snd entries)
  for_ entries \(from, n) -> reportTally vengeanceTally from n

getExplorationDeck :: HasGame m => m [Card]
getExplorationDeck = scenarioFieldMap ScenarioDecks (findWithDefault [] ExplorationDeck)

setExplorationDeck :: ReverseQueue m => [Card] -> m ()
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

eachUnpoisoned :: HasGame m => (InvestigatorId -> m ()) -> m ()
eachUnpoisoned body = do
  unpoisoned <- getUnpoisoned
  for_ unpoisoned body

eachPoisoned :: HasGame m => (InvestigatorId -> m ()) -> m ()
eachPoisoned body = do
  poisoned <- getPoisoned
  for_ poisoned body

getPoisoned :: HasGame m => m [InvestigatorId]
getPoisoned = do
  inResolution <- getInResolution
  let wrapper = if inResolution then IncludeEliminated else id
  select $ wrapper $ HasMatchingTreachery $ treacheryIs Treacheries.poisoned

getUnpoisoned :: HasGame m => m [InvestigatorId]
getUnpoisoned = do
  inResolution <- getInResolution
  let wrapper = if inResolution then IncludeEliminated else id
  select $ wrapper $ NotInvestigator $ HasMatchingTreachery $ treacheryIs Treacheries.poisoned

getSetAsidePoisoned :: HasGame m => m Card
getSetAsidePoisoned =
  fromJustNote "not enough poison cards"
    . find ((== Treacheries.poisoned) . toCardDef)
    <$> scenarioField ScenarioSetAsideCards

becomePoisoned :: ReverseQueue m => InvestigatorId -> m ()
becomePoisoned iid = do
  poisoned <- getSetAsidePoisoned
  createWeaknessInThreatArea poisoned iid
  addCampaignCardToDeck iid DoNotShuffleIn poisoned

becomePoisonedOr :: ReverseQueue m => InvestigatorId -> m () -> m ()
becomePoisonedOr iid notPoisoned = do
  isPoisoned <- getIsPoisoned iid
  if isPoisoned
    then notPoisoned
    else becomePoisoned iid

data ExploreRule = PlaceExplored | ReplaceExplored
  deriving stock Eq

runExplore
  :: (ReverseQueue m, Sourceable source, AsId investigator, IdOf investigator ~ InvestigatorId)
  => investigator -> source -> m ()
runExplore (asId -> iid) (toSource -> source) = do
  matcher <-
    getLocationOf iid >>= \case
      Just lid -> mapOneOf CardWithPrintedLocationSymbol <$> toConnections lid
      Nothing -> pure $ NotCard AnyCard
  push $ Explore iid source matcher

-- ReplaceExplored should actually place the location on "top"

explore :: ReverseQueue m => InvestigatorId -> Source -> CardMatcher -> ExploreRule -> Int -> m ()
explore iid source cardMatcher exploreRule matchCount = do
  explorationDeck <- getExplorationDeck
  canMove <- iid <=~> InvestigatorCanMove
  mlid <- getLocationOf iid
  let
    cardMatcher' = CardWithOneOf [CardWithType EnemyType, CardWithType TreacheryType, cardMatcher]
    go 0 drawnCards deck = (drawnCards, deck, Nothing)
    go n drawnCards deck =
      case deck of
        [] -> (drawnCards, [], Nothing)
        (x : xs)
          | cardMatch x cardMatcher' ->
              if cdCardType (toCardDef x) == LocationType
                then go (n - 1) (drawnCards <> [x]) xs
                else (drawnCards, xs, Just x)
        (x : xs) -> go n (drawnCards <> [x]) xs
    (drawn, rest, mhazard) = go matchCount [] explorationDeck
  case mhazard of
    Just x -> do
      focusCards (drawn <> [x]) do
        chooseTargetM iid [x] \_ -> do
          unfocusCards
      -- Perils of Yoth will handle this case
      if toCardDef x == Treacheries.perilsOfYoth
        then do
          setScenarioDeck ExplorationDeck (drawn <> rest) -- unshuffled in case we continue
        else do
          unless (null drawn) do
            deck' <- shuffle (drawn <> rest)
            setScenarioDeck ExplorationDeck deck'
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

      checkAfter $ Window.Explored iid mlid (Failure x)
    Nothing -> do
      let (matched, notMatched) = partition (`cardMatch` cardMatcher') drawn
      case matched of
        [] -> unless (null drawn) do
          focusCards drawn do
            chooseOneM iid do
              labeledI "noMatchesFound" do
                unfocusCards
                deck' <- shuffle (drawn <> rest)
                setScenarioDeck ExplorationDeck deck'
        [x] -> do
          deck' <- if null notMatched then pure rest else shuffle $ rest <> notMatched
          focusCards (notMatched <> [x]) do
            chooseTargetM iid [x] \_ -> do
              unfocusCards
              setScenarioDeck ExplorationDeck deck'

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

              updateHistory iid $ HistoryItem HistorySuccessfulExplore True
              -- done before the move so trail of the dead handle binoculars check correctly
              checkAfter $ Window.Explored iid mlid (Success lid)
              when (canMove && exploreRule == PlaceExplored) $ moveTo source iid lid
              when (exploreRule == ReplaceExplored) do
                setGlobal lid "replacedIsRevealed" replacedIsRevealed
                setGlobal lid "replacedIsWithoutClues" replacedIsWithoutClues
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
              [ mkAfter $ Window.Explored iid mlid (Success lid)
              | lid <- locations
              ]

getVengeancePoints :: (HasCallStack, ConvertToCard c, HasGame m) => c -> m (Maybe Int)
getVengeancePoints c = do
  card <- convertToCard c
  mods <- getModifiers card
  if ScenarioModifier "noVengeance" `elem` mods
    then pure Nothing
    else getCardField cdVengeancePoints card

exploreAction :: Cost -> AbilityType
exploreAction cost = ActionAbility #explore Nothing (ActionCost 1 <> cost)

exploreAction_ :: AbilityType
exploreAction_ = exploreAction mempty

cancelExplore :: ReverseQueue m => Sourceable source => source -> m ()
cancelExplore source = push $ CancelNext (toSource source) ExploreMessage

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "theForgottenAge" a

pickSupplies :: ReverseQueue m => InvestigatorId -> Bool -> Metadata -> [Supply] -> Message -> m ()
pickSupplies iid resupply metadata supplies cont = do
  let pointsRemaining = findWithDefault 0 iid (supplyPoints metadata)
  when (pointsRemaining > 0) do
    player <- getPlayer iid
    chosenSupplies <- field InvestigatorSupplies iid
    let
      availableSupply s = s `notElem` chosenSupplies || s `elem` [Provisions, Medicine, Gasoline]
      affordableSupplies = filter ((<= pointsRemaining) . supplyCost) supplies
      availableSupplies = filter availableSupply affordableSupplies
      choices = Label "$done" [] : map (\s -> supplyLabel s [PickSupply iid s, cont]) availableSupplies
    push $ Ask player $ PickSupplies {..}

supplyLabel :: Supply -> [Message] -> UI Message
supplyLabel s = case s of
  Provisions -> go "provisions"
  Medicine -> go "medicine"
  Gasoline -> go "gasoline"
  Rope -> go "rope"
  Blanket -> go "blanket"
  Canteen -> go "canteen"
  Torches -> go "torches"
  Compass -> go "compass"
  Map -> go "map"
  Binoculars -> go "binoculars"
  Chalk -> go "chalk"
  Pendant -> go "pendant"
  Pocketknife -> go "pocketknife"
  Pickaxe -> go "pickaxe"
  KeyOfEztli -> go "keyOfEztli"
  MysteriousScepter -> go "mysteriousScepter"
  StickyGoop -> go "stickyGoop"
  Journal -> go "journal"
  Satchel -> go "satchel"
 where
  go label =
    campaignI18n
      $ let toKey suffix = "$" <> ikey ("supplies." <> label <> "." <> suffix)
         in TooltipLabel (toKey "name") (Tooltip (toKey "tooltip"))

useSupply :: ReverseQueue m => InvestigatorId -> Supply -> m ()
useSupply iid s = push $ UseSupply iid s

exploreTest
  :: (Sourceable source, Targetable target, ReverseQueue m)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> target
  -> SkillType
  -> GameCalculation
  -> m ()
exploreTest sid iid (toSource -> source) (toTarget -> target) sType n =
  push
    $ BeginSkillTest
    $ (initSkillTest sid iid source target sType (SkillTestDifficulty n))
      { skillTestAction = Just #explore
      }

isHarbinger :: EnemyCreation msg -> Bool
isHarbinger c =
  cardMatch c.card
    $ mapOneOf cardIs [Enemies.harbingerOfValusia, Enemies.harbingerOfValusiaTheSleeperReturns]

whenHarbingerHasEnteredPlay
  :: (Applicative m, Entity sc, EntityAttrs sc ~ ScenarioAttrs) => sc -> m () -> m ()
whenHarbingerHasEnteredPlay sc action = do
  when (Scenario.getMetaKeyDefault "harbingerEnteredPlay" False (toAttrs sc)) action

setHarbingerHasEnteredPlay :: (Entity sc, EntityAttrs sc ~ ScenarioAttrs) => sc -> sc
setHarbingerHasEnteredPlay = overAttrs (setMetaKey "harbingerEnteredPlay" True)
