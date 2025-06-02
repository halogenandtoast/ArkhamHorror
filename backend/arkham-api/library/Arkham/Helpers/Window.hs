{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -O0 #-}

module Arkham.Helpers.Window where

import Arkham.Ability.Types
import Arkham.Asset.Types (Field (..))
import Arkham.Asset.Types qualified as Field
import Arkham.Attack.Types
import Arkham.Card
import Arkham.ChaosToken.Types
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Cost.Status
import Arkham.Deck
import Arkham.Effect.Types (Field (..))
import Arkham.Event.Types qualified as Field
import {-# SOURCE #-} Arkham.Game (abilityMatches)
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Act (actMatches)
import {-# SOURCE #-} Arkham.Helpers.Action (actionMatches)
import Arkham.Helpers.Card (cardListMatches, extendedCardMatch)
import Arkham.Helpers.ChaosToken (matchChaosToken)
import {-# SOURCE #-} Arkham.Helpers.Criteria (passesCriteria)
import Arkham.Helpers.Damage (damageEffectMatches, damageTypeMatches)
import Arkham.Helpers.Deck (deckMatch)
import Arkham.Helpers.Defeat (defeatedByMatches)
import {-# SOURCE #-} Arkham.Helpers.Enemy (enemyAttackMatches)
import Arkham.Helpers.GameValue (gameValueMatches)
import {-# SOURCE #-} Arkham.Helpers.Investigator (matchWho)
import Arkham.Helpers.Location (locationMatches)
import Arkham.Helpers.Phase (matchPhase)
import {-# SOURCE #-} Arkham.Helpers.Playable (getIsPlayable)
import Arkham.Helpers.Ref (sourceToMaybeCard)
import {-# SOURCE #-} Arkham.Helpers.SkillTest (skillTestMatches, skillTestValueMatches)
import Arkham.Helpers.SkillType (skillTypeMatches)
import Arkham.Helpers.Source (sourceMatches)
import Arkham.Helpers.Target (targetListMatches, targetMatches)
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Message
import Arkham.Prelude
import Arkham.Projection
import Arkham.SkillTest.Base (SkillTest (..))
import Arkham.SkillTest.Type
import Arkham.Source
import Arkham.Target
import Arkham.Timing (Timing)
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Treachery.Types (Field (..))
import Arkham.Window
import Arkham.Window qualified as Window
import Arkham.Zone
import Data.Map.Monoidal.Strict qualified as MonoidalMap
import Data.Map.Strict qualified as Map

checkWindow :: HasGame m => Window -> m Message
checkWindow = checkWindows . pure

checkAfter :: HasGame m => WindowType -> m Message
checkAfter = checkWindows . pure . mkAfter

checkWhen :: HasGame m => WindowType -> m Message
checkWhen = checkWindows . pure . mkWhen

checkWindows :: HasGame m => [Window] -> m Message
checkWindows windows' = do
  -- TODO: We don't want to check eliminated investigators except for the InvestigatorEliminated window
  mBatchId <- getCurrentBatchId
  pure $ CheckWindows $ map (\w -> w {windowBatchId = windowBatchId w <|> mBatchId}) windows'

windows :: [WindowType] -> [Message]
windows windows' = [CheckWindows $ map (mkWindow timing) windows' | timing <- [#when, #at, #after]]

fromWindows :: HasGame m => ([Window] -> a) -> m a
fromWindows f = f . concat <$> getWindowStack

allWindows :: HasGame m => m [Window]
allWindows = fromWindows id

wouldWindows :: MonadRandom m => WindowType -> m (BatchId, [Message])
wouldWindows window = do
  batchId <- getRandom
  pure
    ( batchId
    , [ CheckWindows [Window timing window (Just batchId)]
      | timing <- [Timing.When, Timing.AtIf, Timing.After]
      ]
    )

frame :: WindowType -> (Message, Message, Message)
frame window =
  let (whenWindow, atIfWindow, afterWindow) = timings window
   in (CheckWindows [whenWindow], CheckWindows [atIfWindow], CheckWindows [afterWindow])

timings :: WindowType -> (Window, Window, Window)
timings wType = (mkWhen wType, mkAtIf wType, mkAfter wType)

batchedTimings :: BatchId -> WindowType -> (Window, Window, Window)
batchedTimings batchId wType = case timings wType of
  (whenWindow, atIfWindow, afterWindow) ->
    ( whenWindow {windowBatchId = Just batchId}
    , atIfWindow {windowBatchId = Just batchId}
    , afterWindow {windowBatchId = Just batchId}
    )

doFrame :: Message -> WindowType -> [Message]
doFrame msg window =
  let (before, atIf, after) = frame window
   in [before, atIf, Do msg, after]

doBatch :: BatchId -> Message -> WindowType -> [Message]
doBatch batchId msg window =
  let (before, atIf, after) = frame window
   in [before, atIf, DoBatch batchId msg, after]

pushBatch :: HasQueue Message m => BatchId -> Message -> m ()
pushBatch batchId msg = push $ Would batchId [msg]

pushBatched :: HasQueue Message m => BatchId -> [Message] -> m ()
pushBatched batchId msgs = push $ Would batchId msgs

wouldDo :: (MonadRandom m, HasQueue Message m) => Message -> WindowType -> WindowType -> m ()
wouldDo msg wouldWindow window = do
  (batchId, wouldWindowsMsgs) <- wouldWindows wouldWindow
  let framed = doBatch batchId msg window
  push $ Would batchId $ wouldWindowsMsgs <> framed

{- | Take a message which would operate on some value n and instead expand the
windows to add a single one at a time
-}
wouldDoEach
  :: (MonadRandom m, HasQueue Message m)
  => Int
  -> Message
  -> WindowType -- outer would window
  -> WindowType -- would window
  -> WindowType -- outer window
  -> WindowType -- window
  -> m ()
wouldDoEach n msg outerWouldWindow wouldWindow outerWindow window = do
  (outerBatchId, outerWouldWindowsMsgs) <- wouldWindows outerWouldWindow
  let (outerBefore, outerAtIf, outerAfter) = frame outerWindow
  frames <- replicateM n do
    (innerBatchId, innerWouldWindowsMsgs) <- wouldWindows wouldWindow
    let framed = doFrame msg window
    pure $ Would innerBatchId $ innerWouldWindowsMsgs <> framed

  push
    $ Would outerBatchId
    $ outerWouldWindowsMsgs
    <> [outerBefore, outerAtIf]
    <> frames
    <> [outerAfter]

splitWithWindows :: Message -> [WindowType] -> [Message]
splitWithWindows msg ws = [CheckWindows $ map mkWhen ws] <> [msg] <> [CheckWindows $ map mkAfter ws]

discoveredClues :: HasCallStack => [Window] -> Int
discoveredClues =
  fromMaybe (error "missing discovery") . asum . map \case
    (windowType -> Window.DiscoverClues _ _ _ n) -> Just n
    (windowType -> Window.WouldDiscoverClues _ _ _ n) -> Just n
    _ -> Nothing

discoveredCluesAt :: HasCallStack => [Window] -> (LocationId, Int)
discoveredCluesAt =
  fromMaybe (error "missing discovery") . asum . map \case
    (windowType -> Window.DiscoverClues _ lid _ n) -> Just (lid, n)
    (windowType -> Window.WouldDiscoverClues _ lid _ n) -> Just (lid, n)
    _ -> Nothing

discoveredLocation :: HasCallStack => [Window] -> LocationId
discoveredLocation =
  fromMaybe (error "missing discovery") . asum . map \case
    (windowType -> Window.DiscoverClues _ lid _ _) -> Just lid
    (windowType -> Window.DiscoveringLastClue _ lid) -> Just lid
    _ -> Nothing

locationLeavingPlay :: HasCallStack => [Window] -> LocationId
locationLeavingPlay =
  fromMaybe (error "missing locationLeavingPlay") . asum . map \case
    (windowType -> Window.LeavePlay (LocationTarget lid)) -> Just lid
    _ -> Nothing

assetLeavingPlay :: HasCallStack => [Window] -> AssetId
assetLeavingPlay =
  fromMaybe (error "missing assetLeavingPlay") . asum . map \case
    (windowType -> Window.LeavePlay (AssetTarget aid)) -> Just aid
    _ -> Nothing

maybeDiscoveredLocation :: [Window] -> Maybe LocationId
maybeDiscoveredLocation =
  asum . map \case
    (windowType -> Window.DiscoverClues _ lid _ _) -> Just lid
    _ -> Nothing

engagedEnemy :: HasCallStack => [Window] -> EnemyId
engagedEnemy =
  fromMaybe (error "missing enemy") . asum . map \case
    (windowType -> Window.EnemyEngaged _ eid) -> Just eid
    _ -> Nothing

windowSkillTestId :: HasCallStack => [Window] -> SkillTestId
windowSkillTestId =
  fromMaybe (error "missing enemy") . asum . map \case
    (windowType -> Window.AttemptToEvadeEnemy sid _ _) -> Just sid
    (windowType -> Window.InitiatedSkillTest st) -> Just st.id
    _ -> Nothing

enteringEnemy :: HasCallStack => [Window] -> EnemyId
enteringEnemy =
  fromMaybe (error "missing enemy") . asum . map \case
    (windowType -> Window.EnemyEnters eid _) -> Just eid
    _ -> Nothing

entering :: HasCallStack => [Window] -> LocationId
entering =
  fromMaybe (error "missing enter window") . asum . map \case
    (windowType -> Window.Entering _ lid) -> Just lid
    _ -> Nothing

attachedCard :: HasCallStack => [Window] -> Card
attachedCard =
  fromMaybe (error "missing Attach card window") . asum . map \case
    (windowType -> Window.AttachCard _ card _) -> Just card
    _ -> Nothing

healedAmount :: [Window] -> Int
healedAmount = sum . mapMaybe toHealedAmount
 where
  toHealedAmount = \case
    (windowType -> Healed _ _ _ n) -> Just n
    _ -> Nothing

discoveredLocationAndClues :: HasCallStack => [Window] -> (LocationId, Int)
discoveredLocationAndClues =
  fromMaybe (error "missing discovery") . asum . map \case
    (windowType -> Window.DiscoverClues _ lid _ n) -> Just (lid, n)
    _ -> Nothing

defeatedEnemy :: HasCallStack => [Window] -> EnemyId
defeatedEnemy =
  fromMaybe (error "missing enemy") . asum . map \case
    (windowType -> Window.EnemyDefeated _ _ eid) -> Just eid
    _ -> Nothing

attackedEnemy :: HasCallStack => [Window] -> EnemyId
attackedEnemy =
  fromMaybe (error "missing enemy") . asum . map \case
    (windowType -> Window.EnemyAttacked _ _ eid) -> Just eid
    (windowType -> Window.SuccessfulAttackEnemy _ _ eid _) -> Just eid
    (windowType -> Window.FailAttackEnemy _ eid _) -> Just eid
    _ -> Nothing

attackSource :: HasCallStack => [Window] -> Source
attackSource =
  fromMaybe (error "missing enemy") . asum . map \case
    (windowType -> Window.EnemyAttacked _ source _) -> Just source
    _ -> Nothing

evadedEnemy :: HasCallStack => [Window] -> EnemyId
evadedEnemy =
  fromMaybe (error "missing enemy") . asum . map \case
    (windowType -> Window.EnemyEvaded _ eid) -> Just eid
    _ -> Nothing

fromAsset :: HasCallStack => [Window] -> AssetId
fromAsset =
  fromMaybe (error "missing asset") . asum . map \case
    (windowType -> Window.AttackOrEffectSpentLastUse _ (AssetTarget aid) _) -> Just aid
    _ -> Nothing

spawnedEnemy :: HasCallStack => [Window] -> EnemyId
spawnedEnemy =
  fromMaybe (error "missing enemy") . asum . map \case
    (windowType -> Window.EnemySpawns eid _) -> Just eid
    (windowType -> Window.EnemyAttemptsToSpawnAt eid _) -> Just eid
    _ -> Nothing

placedTokens :: Token -> [Window] -> Int
placedTokens _ [] = 0
placedTokens t ((windowType -> Window.PlacedToken _ _ token n) : xs) | token == t = n + placedTokens t xs
placedTokens t ((windowType -> Window.InvestigatorPlacedFromTheirPool _ _ _ token n) : xs) | token == t = n + placedTokens t xs
placedTokens t (_ : xs) = placedTokens t xs

cardPlayed :: HasCallStack => [Window] -> Card
cardPlayed [] = error "missing play card window"
cardPlayed ((windowType -> Window.PlayCard _ c) : _) = c.card
cardPlayed (_ : xs) = cardPlayed xs

data DrawnCard = DrawnCard
  { card :: Card
  , drawnBy :: InvestigatorId
  , drawnFrom :: DeckSignifier
  }

type instance Element DrawnCard = Card

instance MonoFoldable DrawnCard where
  ofoldr f b x = f x.card b
  ofoldl' f a x = f a x.card
  otoList x = [x.card]
  ofoldMap f x = f x.card
  ofoldr1Ex f x = f x.card x.card
  ofoldl1Ex' f x = f x.card x.card

drawnCard :: HasCallStack => [Window] -> DrawnCard
drawnCard [] = error "missing play card window"
drawnCard ((windowType -> Window.DrawCard iid c deck) : _) = DrawnCard c iid deck
drawnCard (_ : xs) = drawnCard xs

cardDrawn :: HasCallStack => [Window] -> Card
cardDrawn [] = error "missing play card window"
cardDrawn ((windowType -> Window.DrawCard _ c _) : _) = c
cardDrawn (_ : xs) = cardDrawn xs

getPlayedEvent :: [Window] -> EventId
getPlayedEvent = \case
  [] -> error "impossible"
  ((windowType -> Window.PlayEventDiscarding _ eventId) : _) -> eventId
  ((windowType -> Window.PlayEvent _ eventId) : _) -> eventId
  (_ : rest) -> getPlayedEvent rest

cardDiscarded :: HasCallStack => [Window] -> Card
cardDiscarded [] = error "missing play card window"
cardDiscarded ((windowType -> Window.DiscardedFromHand _ _ c) : _) = c
cardDiscarded ((windowType -> Window.Discarded _ _ c) : _) = c
cardDiscarded (_ : xs) = cardDiscarded xs

cardsDiscarded :: HasCallStack => [Window] -> [Card]
cardsDiscarded [] = []
cardsDiscarded ((windowType -> Window.DiscardedFromHand _ _ c) : ws) = c : cardsDiscarded ws
cardsDiscarded ((windowType -> Window.Discarded _ _ c) : ws) = c : cardsDiscarded ws
cardsDiscarded (_ : xs) = cardsDiscarded xs

cardDrawnBy :: HasCallStack => [Window] -> (InvestigatorId, Card)
cardDrawnBy [] = error "missing play card window"
cardDrawnBy ((windowType -> Window.DrawCard iid c _) : _) = (iid, c)
cardDrawnBy (_ : xs) = cardDrawnBy xs

cardsDrawn :: [Window] -> [Card]
cardsDrawn [] = []
cardsDrawn ((windowType -> Window.DrawCards _ cs) : rest) = cs <> cardsDrawn rest
cardsDrawn (_ : xs) = cardsDrawn xs

damagedInvestigator :: [Window] -> InvestigatorId
damagedInvestigator [] = error "no damaged investigator"
damagedInvestigator ((windowType -> Window.WouldTakeDamageOrHorror _ (InvestigatorTarget iid) _ _) : _) = iid
damagedInvestigator (_ : xs) = damagedInvestigator xs

dealtDamage :: [Window] -> Int
dealtDamage [] = 0
dealtDamage ((windowType -> Window.WouldTakeDamageOrHorror _ _ n _) : _) = n
dealtDamage (_ : xs) = dealtDamage xs

dealtHorror :: [Window] -> Int
dealtHorror [] = 0
dealtHorror ((windowType -> Window.WouldTakeDamageOrHorror _ _ _ n) : _) = n
dealtHorror (_ : xs) = dealtDamage xs

wouldRevealChaosToken :: HasCallStack => [Window] -> InvestigatorId
wouldRevealChaosToken =
  fromMaybe (error "missing discovery") . asum . map \case
    (windowType -> Window.WouldRevealChaosToken _ who) -> Just who
    (windowType -> Window.WouldRevealChaosTokens _ who) -> Just who
    _ -> Nothing

getDrawSource :: HasCallStack => [Window] -> Source
getDrawSource = fromMaybe (error "missing draw source") . getMaybeDrawSource

getMaybeDrawSource :: [Window] -> Maybe Source
getMaybeDrawSource [] = Nothing
getMaybeDrawSource ((windowType -> Window.WouldRevealChaosToken drawSource _) : _) = Just drawSource
getMaybeDrawSource ((windowType -> Window.WouldRevealChaosTokens drawSource _) : _) = Just drawSource
getMaybeDrawSource (_ : rest) = getMaybeDrawSource rest

enters
  :: (Be investigator InvestigatorMatcher, Be location LocationMatcher)
  => Timing
  -> investigator
  -> location
  -> WindowMatcher
enters timing investigator location = Enters timing (be investigator) (be location)

defeated :: Timing -> EnemyMatcher -> WindowMatcher
defeated timing matcher = Arkham.Matcher.EnemyDefeated timing Anyone ByAny matcher

moves
  :: (Be who InvestigatorMatcher, Be from LocationMatcher, Be to LocationMatcher)
  => Timing
  -> who
  -> from
  -> to
  -> WindowMatcher
moves timing who source destination =
  Arkham.Matcher.Moves timing (be who) AnySource (be source) (be destination)

getRevealedChaosTokens :: [Window] -> [ChaosToken]
getRevealedChaosTokens = \case
  [] -> []
  ((windowType -> Window.SkillTestEnded st) : _) -> st.revealedChaosTokens
  ((windowType -> Window.RevealChaosTokensDuringSkillTest _ _ ts) : _) -> ts
  ((windowType -> Window.RevealChaosToken _ t) : rest) -> t : getRevealedChaosTokens rest
  (_ : rest) -> getRevealedChaosTokens rest

getRevealedLocation :: [Window] -> LocationId
getRevealedLocation = \case
  [] -> error "No location revealed"
  ((windowType -> Window.RevealLocation _ lid) : _) -> lid
  (_ : rest) -> getRevealedLocation rest

getChaosToken :: HasCallStack => [Window] -> ChaosToken
getChaosToken = \case
  [] -> error "No chaos token drawn"
  ((windowType -> Window.RevealChaosToken _ token) : _) -> token
  ((windowType -> Window.ResolvesChaosToken _ token) : _) -> token
  (_ : rest) -> getChaosToken rest

getThatEnemy :: [Window] -> Maybe EnemyId
getThatEnemy = \case
  [] -> Nothing
  ((windowType -> Window.WouldReady (EnemyTarget eid)) : _) -> Just eid
  ((windowType -> Window.WouldPlaceDoom _ (EnemyTarget eid) _) : _) -> Just eid
  (_ : rest) -> getThatEnemy rest

getAttackDetails :: HasCallStack => [Window] -> EnemyAttackDetails
getAttackDetails = \case
  [] -> error "No attack details"
  ((windowType -> Window.EnemyWouldAttack details) : _) -> details
  ((windowType -> Window.EnemyAttacks details) : _) -> details
  ((windowType -> Window.EnemyAttacksEvenIfCancelled details) : _) -> details
  (_ : rest) -> getAttackDetails rest

getInvestigatedLocation :: HasCallStack => [Window] -> LocationId
getInvestigatedLocation = \case
  [] -> error "No fail or pass skill test"
  ((windowType -> Window.FailInvestigationSkillTest _ lid _) : _) -> lid
  ((windowType -> Window.PassInvestigationSkillTest _ lid _) : _) -> lid
  (_ : rest) -> getInvestigatedLocation rest

getPassedBy :: [Window] -> Int
getPassedBy = \case
  [] -> 0
  ((windowType -> Window.PassInvestigationSkillTest _ _ n) : _) -> n
  ((windowType -> Window.SuccessfulEvadeEnemy _ _ n) : _) -> n
  ((windowType -> Window.PassSkillTest _ _ _ n) : _) -> n
  (_ : rest) -> getPassedBy rest

getEnemy :: [Window] -> EnemyId
getEnemy = \case
  ((windowType -> Window.EnemySpawns eid _) : _) -> eid
  ((windowType -> Window.EnemyDefeated _ _ eid) : _) -> eid
  (_ : rest) -> getEnemy rest
  _ -> error "invalid window"

getLocation :: [Window] -> Maybe LocationId
getLocation = \case
  [] -> Nothing
  ((windowType -> Window.EnemyEnters _ lid) : _) -> Just lid
  ((windowType -> Window.EnemyLeaves _ lid) : _) -> Just lid
  (_ : rest) -> getLocation rest

getEnemies :: [Window] -> [EnemyId]
getEnemies = \case
  [] -> []
  ((windowType -> Window.EnemyEnters eid _) : rest) -> eid : getEnemies rest
  ((windowType -> Window.EnemyLeaves eid _) : rest) -> eid : getEnemies rest
  (_ : rest) -> getEnemies rest

damagedEnemy :: [Window] -> EnemyId
damagedEnemy = \case
  ((windowType -> Window.DealtDamage _ _ (EnemyTarget eid) _) : _) -> eid
  _ -> error "Expected DealtDamage window"

getDamageSource :: HasCallStack => [Window] -> Source
getDamageSource = \case
  [] -> error "No damage"
  ((windowType -> Window.DealtDamage source _ _ _) : _) -> source
  ((windowType -> Window.DealtExcessDamage source _ _ _) : _) -> source
  (_ : rest) -> getDamageSource rest

getDamageSourceEnemy :: HasCallStack => [Window] -> EnemyId
getDamageSourceEnemy ws = case (getDamageSource ws).enemy of
  Nothing -> error "Source was not enemy"
  Just eid -> eid

getDamageOrHorrorSource :: HasCallStack => [Window] -> Source
getDamageOrHorrorSource = \case
  [] -> error "No damage"
  ((windowType -> Window.DealtDamage source _ _ _) : _) -> source
  ((windowType -> Window.DealtHorror source _ _) : _) -> source
  ((windowType -> Window.DealtExcessDamage source _ _ _) : _) -> source
  (_ : rest) -> getDamageSource rest

getTotalDamageAmounts :: Targetable target => target -> [Window] -> Map Source (Int, Int)
getTotalDamageAmounts target =
  Map.map (bimap getSum getSum) . MonoidalMap.getMonoidalMap . foldMap \case
    (windowType -> Window.DealtDamage source _ (isTarget target -> True) d) -> MonoidalMap.singleton source (Sum d, Sum 0)
    (windowType -> Window.DealtHorror source (isTarget target -> True) h) -> MonoidalMap.singleton source (Sum 0, Sum h)
    (windowType -> Window.DealtExcessDamage source _ (isTarget target -> True) d) -> MonoidalMap.singleton source (Sum d, Sum 0)
    _ -> mempty

replaceWindow
  :: (HasCallStack, HasQueue Message m) => (Window -> Bool) -> (Window -> Window) -> m ()
replaceWindow f wf = do
  replaceMessageMatching
    \case
      CheckWindows ws -> any f ws
      Do (CheckWindows ws) -> any f ws
      _ -> False
    \case
      CheckWindows ws -> [CheckWindows $ map (\w -> if f w then wf w else w) ws]
      Do (CheckWindows ws) -> [Do (CheckWindows $ map (\w -> if f w then wf w else w) ws)]
      _ -> error "impossible"

replaceWindowMany
  :: (HasCallStack, HasQueue Message m) => (WindowType -> Bool) -> (WindowType -> [WindowType]) -> m ()
replaceWindowMany f wf = do
  replaceAllMessagesMatching
    \case
      CheckWindows ws -> any (f . windowType) ws
      Do (CheckWindows ws) -> any (f . windowType) ws
      _ -> False
    \case
      CheckWindows ws ->
        [ CheckWindows
            $ concatMap
              (\w -> if f w.kind then map (`replaceWindowType` w) (wf w.kind) else [w])
              ws
        ]
      Do (CheckWindows ws) ->
        [ Do
            ( CheckWindows
                $ concatMap
                  (\w -> if f w.kind then map (`replaceWindowType` w) (wf w.kind) else [w])
                  ws
            )
        ]
      _ -> error "impossible"

windowSkillTest :: [Window] -> Maybe SkillTest
windowSkillTest = \case
  [] -> Nothing
  ((windowType -> Window.InitiatedSkillTest st) : _) -> Just st
  (_ : rest) -> windowSkillTest rest

getCommittedCard :: [Window] -> Card
getCommittedCard [] = error "missing card"
getCommittedCard ((windowType -> Window.CommittedCard _ c) : _) = c
getCommittedCard (_ : ws) = getCommittedCard ws

getWindowAsset :: [Window] -> Maybe AssetId
getWindowAsset [] = Nothing
getWindowAsset ((windowType -> Window.ActivateAbility _ _ ability) : xs) = case abilitySource ability of
  AssetSource aid -> Just aid
  _ -> getWindowAsset xs
getWindowAsset (_ : xs) = getWindowAsset xs

windowMatches
  :: (HasGame m, HasCallStack)
  => InvestigatorId
  -> Source
  -> Window
  -> Matcher.WindowMatcher
  -> m Bool
windowMatches _ _ (windowType -> Window.DoNotCheckWindow) _ = pure True
windowMatches iid rawSource window'@(windowTiming &&& windowType -> (timing', wType)) umtchr = do
  (source, mcard) <-
    case rawSource of
      BothSource s (CardIdSource cid) -> do
        card <- getCard cid
        pure (s, Just (card, UnpaidCost NeedsAction))
      _ -> pure (rawSource, Nothing)

  let noMatch = pure False
  let isMatch' = pure True
  let guardTiming t body = if timing' == t then body wType else noMatch
  let mtchr = Matcher.replaceYouMatcher iid umtchr
  case mtchr of
    Matcher.NotWindow inner -> not <$> windowMatches iid rawSource window' inner
    Matcher.TakeControlOfKey timing whoMatcher _keyMatcher -> guardTiming timing \case
      Window.TakeControlOfKey who _ -> matchWho iid who whoMatcher
      _ -> noMatch
    Matcher.TakeControlOfClues timing whoMatcher sourceMatcher -> guardTiming timing \case
      Window.TakeControlOfClues who source' _ -> do
        andM
          [ matchWho iid who whoMatcher
          , sourceMatches source' sourceMatcher
          ]
      _ -> noMatch
    Matcher.VehicleEnters timing assetMatcher whereMatcher -> guardTiming timing \case
      Window.VehicleEnters aid where' -> do
        andM
          [ locationMatches iid source window' where' whereMatcher
          , aid <=~> assetMatcher
          ]
      _ -> noMatch
    Matcher.VehicleLeaves timing assetMatcher whereMatcher -> guardTiming timing \case
      Window.VehicleLeaves aid where' -> do
        andM
          [ locationMatches iid source window' where' whereMatcher
          , aid <=~> assetMatcher
          ]
      _ -> noMatch
    Matcher.WouldPlaceClueOnLocation timing whoMatcher whereMatcher valueMatcher -> guardTiming timing \case
      Window.WouldPlaceClueOnLocation who where' _ n -> do
        andM
          [ matchWho iid who whoMatcher
          , locationMatches iid source window' where' whereMatcher
          , gameValueMatches n valueMatcher
          ]
      _ -> noMatch
    Matcher.WouldAddChaosTokensToChaosBag timing mWhoMatcher valueMatcher face -> guardTiming timing \case
      Window.WouldAddChaosTokensToChaosBag mWho tokens -> do
        let matchCount = count (== face) tokens
        andM
          [ gameValueMatches matchCount valueMatcher
          , maybe
              (pure True)
              (\whoMatcher -> maybe (pure False) (\who -> matchWho iid who whoMatcher) mWho)
              mWhoMatcher
          ]
      _ -> noMatch
    Matcher.RevealChaosTokensDuringSkillTest timing whoMatcher skillTestMatcher chaosTokenMatcher -> guardTiming timing \case
      Window.RevealChaosTokensDuringSkillTest who st chaosTokens -> do
        andM
          [ matchWho iid who whoMatcher
          , skillTestMatches iid source st skillTestMatcher
          , anyM (`matches` Matcher.IncludeSealed chaosTokenMatcher) chaosTokens
          ]
      _ -> noMatch
    Matcher.InvestigatorPlacedFromTheirPool timing whoMatcher sourceMatcher targetMatcher tType -> guardTiming timing \case
      Window.InvestigatorPlacedFromTheirPool who source' target' tType' _ | tType == tType' -> do
        andM
          [ matchWho iid who whoMatcher
          , target' `targetMatches` targetMatcher
          , source' `sourceMatches` sourceMatcher
          ]
      _ -> noMatch
    Matcher.AttachCard timing mWhoMatcher cardMatcher targetMatcher -> guardTiming timing \case
      Window.AttachCard mWho card target -> do
        andM
          [ pure $ card `cardMatch` cardMatcher
          , maybe
              (pure True)
              (\matcher -> maybe (pure False) (\who -> matchWho iid who matcher) mWho)
              mWhoMatcher
          , target `targetMatches` targetMatcher
          ]
      _ -> noMatch
    Matcher.WindowWhen criteria mtchr' -> do
      (&&)
        <$> passesCriteria iid mcard source source [window'] criteria
        <*> windowMatches iid rawSource window' mtchr'
    Matcher.NotAnyWindow -> noMatch
    Matcher.AnyWindow -> isMatch'
    Matcher.FloodLevelChanged timing whereMatcher -> guardTiming timing \case
      Window.FloodLevelChanged where' _ _ -> locationMatches iid source window' where' whereMatcher
      _ -> noMatch
    Matcher.FloodLevelIncreased timing whereMatcher -> guardTiming timing \case
      Window.FloodLevelChanged where' fl1 fl2 | fl2 > fl1 -> locationMatches iid source window' where' whereMatcher
      _ -> noMatch
    Matcher.FirstTimeParleyingThisRound timing whoMatcher -> guardTiming timing \case
      Window.FirstTimeParleyingThisRound who -> matchWho iid who whoMatcher
      _ -> noMatch
    Matcher.ScenarioCountIncremented timing k -> guardTiming timing \case
      Window.ScenarioCountIncremented k' -> pure $ k == k'
      _ -> noMatch
    Matcher.IncreasedAlarmLevel timing whoMatcher -> guardTiming timing \case
      Window.IncreasedAlarmLevel who -> matchWho iid who whoMatcher
      _ -> noMatch
    Matcher.SkillTestStep timing step -> guardTiming timing \case
      Window.SkillTestStep step' -> pure $ step == step'
      _ -> noMatch
    Matcher.PlacedToken timing sourceMatcher targetMatcher token -> guardTiming timing \case
      Window.PlacedToken source' target' token' _ ->
        andM
          [ pure $ token == token'
          , targetMatches target' targetMatcher
          , sourceMatches source' sourceMatcher
          ]
      _ -> noMatch
    Matcher.EntersThreatArea timing whoMatcher cardMatcher -> guardTiming timing \case
      Window.EntersThreatArea who card -> do
        andM
          [ pure $ card `cardMatch` cardMatcher
          , matchWho iid who whoMatcher
          ]
      _ -> noMatch
    Matcher.WouldPayCardCost timing whomatcher cardMatcher -> guardTiming timing \case
      Window.WouldPayCardCost who _ _ card -> do
        andM
          [ pure $ card `cardMatch` cardMatcher
          , matchWho iid who whomatcher
          ]
      _ -> noMatch
    Matcher.AttackOrEffectSpentLastUse timing sourceMatcher targetMatcher uType -> guardTiming timing $ \case
      Window.AttackOrEffectSpentLastUse source' target' uType' | uType == uType' -> do
        andM
          [ sourceMatches source' sourceMatcher
          , targetMatches target' targetMatcher
          ]
      _ -> noMatch
    Matcher.SpentUses timing whoMatcher sourceMatcher uType assetMatcher valueMatcher -> guardTiming timing $ \case
      Window.SpentUses who source' assetId uType' n | uType == uType' -> do
        andM
          [ matchWho iid who whoMatcher
          , sourceMatches source' sourceMatcher
          , assetId <=~> assetMatcher
          , gameValueMatches n valueMatcher
          ]
      _ -> noMatch
    Matcher.WouldSearchDeck timing whoMatcher deckMatcher -> guardTiming timing $ \case
      Window.WouldSearchDeck who deck -> do
        andM
          [ matchWho iid who whoMatcher
          , deckMatch who deck
              $ Matcher.replaceThatInvestigator who deckMatcher
          ]
      _ -> noMatch
    Matcher.SearchedDeck timing whoMatcher deckMatcher -> guardTiming timing $ \case
      Window.SearchedDeck who deck -> do
        andM
          [ matchWho iid who whoMatcher
          , deckMatch iid deck
              $ Matcher.replaceThatInvestigator who deckMatcher
          ]
      _ -> noMatch
    Matcher.WouldLookAtDeck timing whoMatcher deckMatcher -> guardTiming timing $ \case
      Window.WouldLookAtDeck who deck -> do
        andM
          [ matchWho iid who whoMatcher
          , deckMatch who deck
              $ Matcher.replaceThatInvestigator who deckMatcher
          ]
      _ -> noMatch
    Matcher.LookedAtDeck timing whoMatcher deckMatcher -> guardTiming timing $ \case
      Window.LookedAtDeck who deck -> do
        andM
          [ matchWho iid who whoMatcher
          , deckMatch iid deck
              $ Matcher.replaceThatInvestigator who deckMatcher
          ]
      _ -> noMatch
    Matcher.TokensWouldBeRemovedFromChaosBag timing matcher -> guardTiming timing $ \case
      Window.TokensWouldBeRemovedFromChaosBag tokens' -> anyM (`matches` Matcher.InTokenPool matcher) tokens'
      _ -> noMatch
    Matcher.WouldTriggerChaosTokenRevealEffectOnCard whoMatcher cardMatcher tokens ->
      guardTiming Timing.AtIf $ \case
        Window.RevealChaosTokenEffect who token effectId -> do
          cardCode <- field EffectCardCode effectId
          andM
            [ pure $ chaosTokenFace token `elem` tokens
            , pure $ lookupCard cardCode nullCardId `cardMatch` cardMatcher
            , matchWho iid who whoMatcher
            ]
        Window.RevealChaosTokenTreacheryEffect who tokens' treacheryId -> do
          card <- field TreacheryCard treacheryId
          andM
            [ pure $ any ((`elem` tokens) . chaosTokenFace) tokens'
            , pure $ card `cardMatch` cardMatcher
            , matchWho iid who whoMatcher
            ]
        Window.RevealChaosTokenEventEffect who tokens' eventId -> do
          card <- field Field.EventCard eventId
          andM
            [ pure $ any ((`elem` tokens) . chaosTokenFace) tokens'
            , pure $ card `cardMatch` cardMatcher
            , matchWho iid who whoMatcher
            ]
        Window.RevealChaosTokenAssetAbilityEffect who tokens' assetId -> do
          card <- field Field.AssetCard assetId
          andM
            [ pure $ any ((`elem` tokens) . chaosTokenFace) tokens'
            , pure $ card `cardMatch` cardMatcher
            , matchWho iid who whoMatcher
            ]
        _ -> noMatch
    Matcher.GameBegins timing -> guardTiming timing $ pure . (== Window.GameBegins)
    Matcher.InvestigatorTakeDamage timing whoMatcher sourceMatcher ->
      guardTiming timing \case
        Window.TakeDamage source' _ (InvestigatorTarget who) _ ->
          andM
            [ sourceMatches source' sourceMatcher
            , matchWho iid who whoMatcher
            ]
        Window.DealtDamage source' _ (InvestigatorTarget who) _ ->
          andM
            [ sourceMatches source' sourceMatcher
            , matchWho iid who whoMatcher
            ]
        _ -> noMatch
    Matcher.InvestigatorTakeHorror timing whoMatcher sourceMatcher ->
      guardTiming timing $ \case
        Window.TakeHorror source' (InvestigatorTarget who) ->
          andM
            [ matchWho iid who whoMatcher
            , sourceMatches source' sourceMatcher
            ]
        Window.DealtHorror source' (InvestigatorTarget who) _ ->
          andM
            [ matchWho iid who whoMatcher
            , sourceMatches source' sourceMatcher
            ]
        _ -> noMatch
    Matcher.InvestigatorWouldTakeDamage timing whoMatcher sourceMatcher damageTypeMatcher ->
      guardTiming timing $ \case
        Window.WouldTakeDamage source' (InvestigatorTarget who) _ strategy ->
          andM
            [ pure $ damageTypeMatches strategy damageTypeMatcher
            , matchWho iid who whoMatcher
            , sourceMatches source' sourceMatcher
            ]
        Window.WouldTakeDamageOrHorror source' (InvestigatorTarget who) n _ | n > 0 -> do
          andM
            [ matchWho iid who whoMatcher
            , sourceMatches source' sourceMatcher
            ]
        _ -> noMatch
    Matcher.InvestigatorWouldTakeHorror timing whoMatcher sourceMatcher ->
      guardTiming timing $ \case
        Window.WouldTakeHorror source' (InvestigatorTarget who) _ ->
          andM
            [ matchWho iid who whoMatcher
            , sourceMatches source' sourceMatcher
            ]
        Window.WouldTakeDamageOrHorror source' (InvestigatorTarget who) _ n | n > 0 -> do
          andM
            [ matchWho iid who whoMatcher
            , sourceMatches source' sourceMatcher
            ]
        _ -> noMatch
    Matcher.SuccessfullyInvestigatedWithNoClues timing whoMatcher whereMatcher -> guardTiming timing $ \case
      Window.SuccessfullyInvestigateWithNoClues who where' -> do
        andM
          [ matchWho iid who whoMatcher
          , locationMatches iid source window' where' whereMatcher
          ]
      _ -> pure False
    Matcher.LostActions timing whoMatcher sourceMatcher -> guardTiming timing $ \case
      Window.LostActions who source' _ ->
        andM
          [ matchWho iid who whoMatcher
          , sourceMatches source' sourceMatcher
          ]
      _ -> noMatch
    Matcher.ScenarioEvent timing mWhoMatcher eKey -> guardTiming timing \case
      Window.ScenarioEvent eKey' mWho _ ->
        andM
          [ pure $ eKey == eKey'
          , maybe
              (pure True)
              (\matcher -> maybe (pure False) (\who -> matchWho iid who matcher) mWho)
              mWhoMatcher
          ]
      _ -> noMatch
    Matcher.LostResources timing whoMatcher sourceMatcher -> guardTiming timing $ \case
      Window.LostResources who source' _ ->
        andM
          [ matchWho iid who whoMatcher
          , sourceMatches source' sourceMatcher
          ]
      _ -> noMatch
    Matcher.CancelledOrIgnoredCardOrGameEffect sourceMatcher ->
      guardTiming Timing.After $ \case
        Window.CancelledOrIgnoredCardOrGameEffect source' ->
          sourceMatches source' sourceMatcher
        _ -> noMatch
    Matcher.WouldBeShuffledIntoDeck deckMatcher cardMatcher -> case wType of
      Window.WouldBeShuffledIntoDeck deck card
        | cardMatch card cardMatcher ->
            deckMatch iid deck deckMatcher
      _ -> noMatch
    Matcher.AddingToCurrentDepth -> case wType of
      Window.AddingToCurrentDepth -> isMatch'
      _ -> noMatch
    Matcher.DrawingStartingHand timing whoMatcher -> guardTiming timing $ \case
      Window.DrawingStartingHand who -> matchWho iid who whoMatcher
      _ -> noMatch
    Matcher.WouldMoveFromHunter timing enemyMatcher -> guardTiming timing $ \case
      Window.WouldMoveFromHunter eid -> elem eid <$> select enemyMatcher
      _ -> noMatch
    Matcher.MovedFromHunter timing enemyMatcher -> guardTiming timing $ \case
      Window.MovedFromHunter eid -> elem eid <$> select enemyMatcher
      _ -> noMatch
    Matcher.EnemyMovedTo timing locationMatcher movesVia enemyMatcher -> guardTiming timing $ \case
      Window.EnemyMovesTo lid movesVia' eid
        | movesVia == Matcher.MovedViaAny || movesVia == movesVia' ->
            andM [elem eid <$> select enemyMatcher, elem lid <$> select locationMatcher]
      _ -> noMatch
    Matcher.EnemyMoves timing locationMatcher enemyMatcher -> guardTiming timing $ \case
      Window.EnemyMoves eid lid ->
        andM [elem eid <$> select enemyMatcher, elem lid <$> select locationMatcher]
      _ -> noMatch
    Matcher.PlaceUnderneath timing targetMatcher cardMatcher -> guardTiming timing $ \case
      Window.PlaceUnderneath target' card ->
        andM
          [ pure $ cardMatch card cardMatcher
          , targetMatches target' targetMatcher
          ]
      _ -> noMatch
    Matcher.ActivateAbility timing whoMatcher abilityMatcher -> guardTiming timing $ \case
      Window.ActivateAbility who _ ability ->
        -- N.B. For cases like Flare (1) we need to "extend" the ability which
        -- means it will differ from the original, for this reason we can not
        -- use the typicaly `elem ability <$> select abilityMatcher` format and
        -- must instead use the `abilityMatches` function which allows us to
        -- check against the modified ability
        andM
          [ matchWho iid who whoMatcher
          , abilityMatches ability abilityMatcher
          ]
      _ -> noMatch
    Matcher.CommittingCardsFromHandToSkillTestStep timing whoMatcher -> guardTiming timing $ \case
      Window.CommittingCardsFromHandToSkillTestStep who -> matchWho iid who whoMatcher
      _ -> noMatch
    Matcher.CommittedCard timing whoMatcher cardMatcher -> guardTiming timing $ \case
      Window.CommittedCard who card ->
        andM
          [ pure $ cardMatch card cardMatcher
          , matchWho iid who whoMatcher
          ]
      _ -> noMatch
    Matcher.CommittedCards timing whoMatcher cardListMatcher -> guardTiming timing $ \case
      Window.CommittedCards who cards ->
        andM
          [ matchWho iid who whoMatcher
          , cardListMatches cards cardListMatcher
          ]
      _ -> noMatch
    Matcher.EnemyWouldSpawnAt enemyMatcher locationMatcher ->
      case wType of
        Window.EnemyWouldSpawnAt eid lid -> do
          andM
            [ matches eid enemyMatcher
            , lid <=~> locationMatcher
            ]
        _ -> noMatch
    Matcher.EnemyAttemptsToSpawnAt timing enemyMatcher locationMatcher ->
      guardTiming timing $ \case
        Window.EnemyAttemptsToSpawnAt eid locationMatcher' -> do
          case locationMatcher of
            Matcher.LocationNotInPlay -> do
              andM
                [ matches eid enemyMatcher
                , selectNone locationMatcher'
                ]
            other | other == locationMatcher' -> matches eid enemyMatcher
            _ -> noMatch -- TODO: We may need more things here
        _ -> noMatch
    Matcher.TookControlOfAsset timing whoMatcher assetMatcher ->
      guardTiming timing $ \case
        Window.TookControlOfAsset who aid ->
          andM
            [ matchWho iid who whoMatcher
            , elem aid <$> select assetMatcher
            ]
        _ -> noMatch
    Matcher.AssetHealed timing damageType assetMatcher sourceMatcher ->
      guardTiming timing $ \case
        Window.Healed damageType' (AssetTarget assetId) source' _ | damageType == damageType' -> do
          andM
            [ elem assetId <$> select assetMatcher
            , sourceMatches source' sourceMatcher
            ]
        _ -> noMatch
    Matcher.InvestigatorHealed timing damageType whoMatcher sourceMatcher ->
      guardTiming timing $ \case
        Window.Healed damageType' (InvestigatorTarget who) source' _ | damageType == damageType' -> do
          andM
            [matchWho iid who whoMatcher, sourceMatches source' sourceMatcher]
        _ -> noMatch
    Matcher.WouldPerformRevelationSkillTest timing whoMatcher ->
      guardTiming timing $ \case
        Window.WouldPerformRevelationSkillTest who _ -> matchWho iid who whoMatcher
        _ -> noMatch
    Matcher.WouldDrawEncounterCard timing whoMatcher phaseMatcher ->
      guardTiming timing $ \case
        Window.WouldDrawEncounterCard who p ->
          andM [matchWho iid who whoMatcher, matchPhase p phaseMatcher]
        _ -> noMatch
    Matcher.AmongSearchedCards whoMatcher -> case wType of
      Window.AmongSearchedCards _ who -> do
        field InvestigatorSearch who >>= \case
          Nothing -> pure False
          Just search' ->
            andM
              [ maybe False (`elem` search'.allFoundCards) <$> sourceToMaybeCard source
              , matchWho iid who whoMatcher
              ]
      _ -> noMatch
    Matcher.WouldDiscardFromHand timing whoMatcher sourceMatcher ->
      guardTiming timing $ \case
        Window.WouldDiscardFromHand who source' ->
          andM
            [ matchWho iid who whoMatcher
            , sourceMatches source' sourceMatcher
            ]
        _ -> noMatch
    Matcher.Discarded timing mWhoMatcher sourceMatcher cardMatcher ->
      guardTiming timing $ \case
        Window.Discarded mWho source' card ->
          andM
            [ maybe
                (pure True)
                (\matcher -> maybe (pure False) (\who -> matchWho iid who matcher) mWho)
                mWhoMatcher
            , sourceMatches source' sourceMatcher
            , extendedCardMatch card cardMatcher
            ]
        _ -> noMatch
    Matcher.DiscardedFromHand timing whoMatcher sourceMatcher cardMatcher ->
      guardTiming timing $ \case
        Window.DiscardedFromHand who source' card ->
          andM
            [ matchWho iid who whoMatcher
            , sourceMatches source' sourceMatcher
            , extendedCardMatch card cardMatcher
            ]
        _ -> noMatch
    Matcher.AssetWouldBeDiscarded timing assetMatcher -> guardTiming timing $ \case
      Window.WouldBeDiscarded (AssetTarget aid) -> elem aid <$> select assetMatcher
      _ -> noMatch
    Matcher.EventWouldBeDiscarded timing eventMatcher -> guardTiming timing $ \case
      Window.WouldBeDiscarded (EventTarget aid) -> elem aid <$> select eventMatcher
      _ -> noMatch
    Matcher.EnemyWouldBeDiscarded timing enemyMatcher -> guardTiming timing $ \case
      Window.WouldBeDiscarded (EnemyTarget eid) -> elem eid <$> select enemyMatcher
      _ -> noMatch
    Matcher.EnemyDiscarded timing sourceMatcher enemyMatcher -> guardTiming timing $ \case
      Window.EntityDiscarded source' (EnemyTarget eid) ->
        andM
          [ eid <=~> enemyMatcher
          , sourceMatches source' sourceMatcher
          ]
      _ -> noMatch
    Matcher.TreacheryWouldBeDiscarded timing treacheryMatcher -> guardTiming timing $ \case
      Window.WouldBeDiscarded (TreacheryTarget tid) -> elem tid <$> select treacheryMatcher
      _ -> noMatch
    Matcher.TreacheryDiscarded timing sourceMatcher treacheryMatcher -> guardTiming timing $ \case
      Window.EntityDiscarded source' (TreacheryTarget tid) ->
        andM
          [ tid <=~> treacheryMatcher
          , sourceMatches source' sourceMatcher
          ]
      _ -> noMatch
    Matcher.AgendaAdvances timing agendaMatcher -> guardTiming timing $ \case
      Window.AgendaAdvance aid ->
        case agendaMatcher of
          AnyAgenda -> pure True
          _ -> matches aid agendaMatcher
      _ -> noMatch
    Matcher.ActAdvances timing actMatcher -> guardTiming timing $ \case
      Window.ActAdvance aid -> actMatches aid actMatcher
      _ -> noMatch
    Matcher.Exhausts timing whoMatcher targetMatcher -> guardTiming timing \case
      Window.Exhausts target@(AssetTarget aid) -> do
        mController <- field AssetController aid
        case mController of
          Just controller -> do
            andM
              [ matchWho iid controller whoMatcher
              , targetMatches target targetMatcher
              ]
          Nothing -> noMatch
      _ -> noMatch
    Matcher.EnemyExhausts timing enemyMatcher -> guardTiming timing \case
      Window.Exhausts (EnemyTarget eid) -> matches eid enemyMatcher
      _ -> noMatch
    Matcher.MovedBy timing whoMatcher sourceMatcher -> guardTiming timing $ \case
      Window.Moves who source' _ _ ->
        andM
          [ matchWho iid who whoMatcher
          , sourceMatches source' sourceMatcher
          ]
      _ -> noMatch
    Matcher.MovedButBeforeEnemyEngagement timing whoMatcher whereMatcher ->
      guardTiming timing $ \case
        Window.MovedButBeforeEnemyEngagement who locationId ->
          andM
            [ matchWho iid who whoMatcher
            , locationMatches iid source window' locationId whereMatcher
            ]
        _ -> noMatch
    Matcher.InvestigatorDefeated timing defeatedByMatcher whoMatcher ->
      guardTiming timing $ \case
        Window.InvestigatorDefeated defeatedBy who ->
          andM
            [ matchWho iid who whoMatcher
            , defeatedByMatches defeatedBy defeatedByMatcher
            ]
        _ -> noMatch
    Matcher.InvestigatorWouldBeDefeated timing defeatedByMatcher whoMatcher ->
      guardTiming timing $ \case
        Window.InvestigatorWouldBeDefeated defeatedBy who ->
          andM
            [ matchWho iid who whoMatcher
            , defeatedByMatches defeatedBy defeatedByMatcher
            ]
        _ -> noMatch
    Matcher.AgendaWouldAdvance timing advancementReason agendaMatcher ->
      guardTiming timing $ \case
        Window.AgendaWouldAdvance advancementReason' aid | advancementReason == advancementReason' -> do
          matches aid agendaMatcher
        _ -> noMatch
    Matcher.WouldPlaceDoomCounter timing sourceMatcher targetMatcher -> guardTiming timing $ \case
      Window.WouldPlaceDoom source' target _ ->
        andM [targetMatches target targetMatcher, sourceMatches source' sourceMatcher]
      _ -> noMatch
    Matcher.PlacedDoomCounter timing sourceMatcher targetMatcher -> guardTiming timing $ \case
      Window.PlacedDoom source' target _ ->
        andM [targetMatches target targetMatcher, sourceMatches source' sourceMatcher]
      _ -> noMatch
    Matcher.PlacedDoomCounterOnTargetWithNoDoom timing sourceMatcher targetMatcher -> guardTiming timing $ \case
      Window.PlacedDoomCounterOnTargetWithNoDoom source' target _ ->
        andM [targetMatches target targetMatcher, sourceMatches source' sourceMatcher]
      _ -> noMatch
    Matcher.WouldPlaceBreach timing targetMatcher -> guardTiming timing $ \case
      Window.WouldPlaceBreach target -> targetMatches target targetMatcher
      _ -> noMatch
    Matcher.PlacedBreaches timing targetMatcher -> guardTiming timing $ \case
      Window.PlacedBreaches target -> targetMatches target targetMatcher
      _ -> noMatch
    Matcher.PlacedBreach timing targetMatcher -> guardTiming timing $ \case
      Window.PlacedBreach target -> targetMatches target targetMatcher
      _ -> noMatch
    Matcher.WouldRemoveBreach timing targetMatcher -> guardTiming timing $ \case
      Window.WouldRemoveBreach target -> targetMatches target targetMatcher
      _ -> noMatch
    Matcher.RemovedBreaches timing targetMatcher -> guardTiming timing $ \case
      Window.RemovedBreaches target -> targetMatches target targetMatcher
      _ -> noMatch
    Matcher.RemovedBreach timing targetMatcher -> guardTiming timing $ \case
      Window.RemovedBreach target -> targetMatches target targetMatcher
      _ -> noMatch
    Matcher.PlacedCounter timing whoMatcher sourceMatcher counterMatcher valueMatcher ->
      guardTiming timing $ \case
        Window.PlacedHorror source' (InvestigatorTarget iid') n | counterMatcher == Matcher.HorrorCounter -> do
          andM
            [ matchWho iid iid' whoMatcher
            , sourceMatches source' sourceMatcher
            , gameValueMatches n valueMatcher
            ]
        Window.PlacedDamage source' (InvestigatorTarget iid') n | counterMatcher == Matcher.DamageCounter -> do
          andM
            [ matchWho iid iid' whoMatcher
            , sourceMatches source' sourceMatcher
            , gameValueMatches n valueMatcher
            ]
        _ -> noMatch
    Matcher.PlacedCounterOnLocation timing whereMatcher sourceMatcher counterMatcher valueMatcher ->
      guardTiming timing $ \case
        Window.PlacedClues source' (LocationTarget locationId) n | counterMatcher == Matcher.ClueCounter -> do
          andM
            [ locationMatches iid source window' locationId whereMatcher
            , sourceMatches source' sourceMatcher
            , gameValueMatches n valueMatcher
            ]
        Window.PlacedResources source' (LocationTarget locationId) n | counterMatcher == Matcher.ResourceCounter -> do
          andM
            [ locationMatches iid source window' locationId whereMatcher
            , sourceMatches source' sourceMatcher
            , gameValueMatches n valueMatcher
            ]
        Window.PlacedDamage source' (LocationTarget locationId) n | counterMatcher == Matcher.DamageCounter -> do
          andM
            [ locationMatches iid source window' locationId whereMatcher
            , sourceMatches source' sourceMatcher
            , gameValueMatches n valueMatcher
            ]
        _ -> noMatch
    Matcher.PlacedCounterOnEnemy timing enemyMatcher sourceMatcher counterMatcher valueMatcher ->
      guardTiming timing $ \case
        Window.PlacedClues source' (EnemyTarget enemyId) n | counterMatcher == Matcher.ClueCounter -> do
          andM
            [ matches enemyId enemyMatcher
            , sourceMatches source' sourceMatcher
            , gameValueMatches n valueMatcher
            ]
        Window.PlacedDoom source' (EnemyTarget enemyId) n | counterMatcher == Matcher.DoomCounter -> do
          andM
            [ matches enemyId enemyMatcher
            , sourceMatches source' sourceMatcher
            , gameValueMatches n valueMatcher
            ]
        _ -> noMatch
    Matcher.PlacedCounterOnAgenda timing agendaMatcher sourceMatcher counterMatcher valueMatcher ->
      guardTiming timing $ \case
        Window.PlacedDoom source' (AgendaTarget agendaId) n | counterMatcher == Matcher.DoomCounter -> do
          andM
            [ matches agendaId agendaMatcher
            , sourceMatches source' sourceMatcher
            , gameValueMatches n valueMatcher
            ]
        _ -> noMatch
    Matcher.PlacedCounterOnAsset timing assetMatcher sourceMatcher counterMatcher valueMatcher ->
      guardTiming timing $ \case
        Window.PlacedHorror source' (AssetTarget assetId) n | counterMatcher == Matcher.HorrorCounter -> do
          andM
            [ assetId <=~> assetMatcher
            , sourceMatches source' sourceMatcher
            , gameValueMatches n valueMatcher
            ]
        Window.PlacedDamage source' (AssetTarget assetId) n | counterMatcher == Matcher.DamageCounter -> do
          andM
            [ assetId <=~> assetMatcher
            , sourceMatches source' sourceMatcher
            , gameValueMatches n valueMatcher
            ]
        _ -> noMatch
    Matcher.RevealLocation timing whoMatcher locationMatcher ->
      guardTiming timing $ \case
        Window.RevealLocation who locationId ->
          andM
            [ matchWho iid who whoMatcher
            , locationMatches iid source window' locationId locationMatcher
            ]
        _ -> noMatch
    Matcher.FlipLocation timing whoMatcher locationMatcher ->
      guardTiming timing $ \case
        Window.FlipLocation who locationId ->
          andM
            [ matchWho iid who whoMatcher
            , locationMatches iid source window' locationId locationMatcher
            ]
        _ -> noMatch
    Matcher.GameEnds timing -> guardTiming timing (pure . (== Window.EndOfGame))
    Matcher.InvestigatorEliminated timing whoMatcher -> guardTiming timing $ \case
      Window.InvestigatorEliminated who ->
        matchWho iid who (Matcher.IncludeEliminated $ Matcher.replaceYouMatcher iid whoMatcher)
      _ -> noMatch
    Matcher.InvestigatorResigned timing whoMatcher -> guardTiming timing $ \case
      Window.InvestigatorResigned who ->
        matchWho iid who (Matcher.IncludeEliminated $ Matcher.replaceYouMatcher iid whoMatcher)
      _ -> noMatch
    Matcher.PutLocationIntoPlay timing whoMatcher locationMatcher ->
      guardTiming timing $ \case
        Window.PutLocationIntoPlay who locationId ->
          andM
            [ matchWho iid who whoMatcher
            , locationMatches iid source window' locationId locationMatcher
            ]
        _ -> noMatch
    Matcher.LocationEntersPlay timing locationMatcher ->
      guardTiming timing $ \case
        Window.LocationEntersPlay locationId -> locationMatches iid source window' locationId locationMatcher
        _ -> noMatch
    Matcher.PlayerHasPlayableCard costStatus cardMatcher -> do
      -- This is the for the Painted
      -- TODO: do we need to grab the card source?
      -- cards <- filter (/= c) <$> getList cardMatcher
      cards <- select cardMatcher
      anyM (getIsPlayable iid source costStatus [window']) cards
    Matcher.PhaseBegins timing phaseMatcher -> guardTiming timing $ \case
      Window.AnyPhaseBegins -> pure $ phaseMatcher == Matcher.AnyPhase
      Window.PhaseBegins p -> matchPhase p phaseMatcher
      _ -> noMatch
    Matcher.PhaseEnds timing phaseMatcher -> guardTiming timing $ \case
      Window.PhaseEnds p -> matchPhase p phaseMatcher
      _ -> noMatch
    Matcher.PhaseStep timing phaseStepMatcher -> guardTiming timing $ \case
      Window.EnemiesAttackStep -> pure $ phaseStepMatcher == Matcher.EnemiesAttackStep
      Window.HuntersMoveStep -> pure $ phaseStepMatcher == Matcher.HuntersMoveStep
      _ -> noMatch
    Matcher.TurnBegins timing whoMatcher -> guardTiming timing $ \case
      Window.TurnBegins who -> matchWho iid who whoMatcher
      _ -> noMatch
    Matcher.TurnEnds timing whoMatcher -> guardTiming timing $ \case
      Window.TurnEnds who -> matchWho iid who whoMatcher
      _ -> noMatch
    Matcher.TurnWouldEnd timing whoMatcher -> guardTiming timing $ \case
      Window.WouldEndTurn who -> matchWho iid who whoMatcher
      _ -> noMatch
    Matcher.RoundBegins timing -> guardTiming timing (pure . (== Window.AtBeginningOfRound))
    Matcher.RoundEnds timing -> guardTiming timing (pure . (== Window.AtEndOfRound))
    Matcher.Enters timing whoMatcher whereMatcher -> guardTiming timing $ \case
      Window.Entering iid' lid ->
        andM
          [ matchWho iid iid' whoMatcher
          , locationMatches iid source window' lid whereMatcher
          ]
      _ -> noMatch
    Matcher.Leaves timing whoMatcher whereMatcher -> guardTiming timing $ \case
      Window.Leaving iid' lid ->
        andM
          [ matchWho iid iid' whoMatcher
          , locationMatches iid source window' lid whereMatcher
          ]
      _ -> noMatch
    Matcher.Moves timing whoMatcher sourceMatcher fromMatcher toMatcher ->
      guardTiming timing $ \case
        Window.Moves iid' source' mFromLid toLid -> do
          andM
            [ matchWho iid iid' whoMatcher
            , sourceMatches source' sourceMatcher
            , case (fromMatcher, mFromLid) of
                (Matcher.Anywhere, _) -> isMatch'
                (_, Just fromLid) ->
                  locationMatches iid source window' fromLid fromMatcher
                _ -> noMatch
            , locationMatches iid source window' toLid toMatcher
            ]
        _ -> noMatch
    Matcher.WouldMove timing whoMatcher sourceMatcher fromMatcher toMatcher ->
      guardTiming timing $ \case
        Window.WouldMove iid' source' fromLid toLid -> do
          andM
            [ matchWho iid iid' whoMatcher
            , sourceMatches source' sourceMatcher
            , case fromMatcher of
                Matcher.Anywhere -> isMatch'
                _ -> locationMatches iid source window' fromLid fromMatcher
            , locationMatches iid source window' toLid toMatcher
            ]
        _ -> noMatch
    Matcher.MoveAction timing whoMatcher fromMatcher toMatcher ->
      guardTiming timing $ \case
        Window.MoveAction iid' fromLid toLid ->
          andM
            [ matchWho iid iid' whoMatcher
            , locationMatches iid source window' fromLid fromMatcher
            , locationMatches iid source window' toLid toMatcher
            ]
        _ -> noMatch
    Matcher.PerformAction timing whoMatcher actionMatcher -> guardTiming timing $ \case
      Window.PerformAction iid' action ->
        andM [matchWho iid iid' whoMatcher, actionMatches iid action actionMatcher]
      _ -> noMatch
    Matcher.PerformedSameTypeOfAction timing whoMatcher actionMatcher -> guardTiming timing $ \case
      Window.PerformedSameTypeOfAction iid' actions ->
        andM [matchWho iid iid' whoMatcher, anyM (\a -> actionMatches iid a actionMatcher) actions]
      _ -> noMatch
    Matcher.PerformedDifferentTypesOfActionsInARow timing whoMatcher n actionMatcher -> guardTiming timing $ \case
      Window.PerformedDifferentTypesOfActionsInARow iid' m actions
        | m >= n ->
            andM [matchWho iid iid' whoMatcher, anyM (\a -> actionMatches iid a actionMatcher) actions]
      _ -> noMatch
    Matcher.WouldHaveSkillTestResult timing whoMatcher _ skillTestResultMatcher -> do
      -- The #when is questionable, but "Would" based timing really is
      -- only meant to have a When window
      let
        isWindowMatch = \case
          Matcher.ResultOneOf xs -> anyM isWindowMatch xs
          Matcher.FailureResult gameValueMatcher -> guardTiming timing $ \case
            Window.WouldFailSkillTest who n -> andM [matchWho iid who whoMatcher, gameValueMatches n gameValueMatcher]
            _ -> noMatch
          Matcher.SuccessResult gameValueMatcher -> guardTiming timing $ \case
            Window.WouldPassSkillTest who n -> andM [matchWho iid who whoMatcher, gameValueMatches n gameValueMatcher]
            _ -> noMatch
          Matcher.AnyResult -> guardTiming #when $ \case
            Window.WouldFailSkillTest who _ -> matchWho iid who whoMatcher
            Window.WouldPassSkillTest who _ -> matchWho iid who whoMatcher
            _ -> noMatch
      isWindowMatch skillTestResultMatcher
    Matcher.InitiatedSkillTest timing whoMatcher skillTypeMatcher skillValueMatcher skillTestMatcher ->
      guardTiming timing $ \case
        Window.InitiatedSkillTest st -> case skillTestType st of
          SkillSkillTest skillType | skillTypeMatches skillType skillTypeMatcher -> do
            andM
              [ matchWho iid (skillTestInvestigator st) whoMatcher
              , skillTestValueMatches
                  iid
                  (skillTestAction st)
                  (skillTestType st)
                  skillValueMatcher
              , skillTestMatches iid source st skillTestMatcher
              ]
          _ -> noMatch
        _ -> noMatch
    Matcher.SkillTestEnded timing whoMatcher skillTestMatcher -> guardTiming timing $ \case
      Window.SkillTestEnded skillTest ->
        andM
          [ matchWho iid (skillTestInvestigator skillTest) whoMatcher
          , skillTestMatches iid source skillTest skillTestMatcher
          ]
      _ -> noMatch
    Matcher.SkillTestResult timing whoMatcher skillMatcher skillTestResultMatcher ->
      do
        mskillTest <- getSkillTest
        matchSkillTest <- case mskillTest of
          Nothing -> noMatch
          Just st -> skillTestMatches iid source st skillMatcher
        if not matchSkillTest
          then noMatch
          else do
            let
              isWindowMatch = \case
                Matcher.ResultOneOf xs -> anyM isWindowMatch xs
                Matcher.FailureResult gameValueMatcher -> guardTiming timing $ \case
                  Window.FailInvestigationSkillTest who lid n -> case skillMatcher of
                    Matcher.WhileInvestigating whereMatcher ->
                      andM
                        [ matchWho iid who whoMatcher
                        , gameValueMatches n gameValueMatcher
                        , locationMatches iid source window' lid whereMatcher
                        ]
                    _ -> noMatch
                  Window.FailAttackEnemy who enemyId n -> case skillMatcher of
                    Matcher.WhileAttackingAnEnemy enemyMatcher ->
                      andM
                        [ matchWho iid who whoMatcher
                        , gameValueMatches n gameValueMatcher
                        , matches enemyId enemyMatcher
                        ]
                    _ -> noMatch
                  Window.FailEvadeEnemy who enemyId n -> case skillMatcher of
                    Matcher.WhileEvadingAnEnemy enemyMatcher ->
                      andM
                        [ matchWho iid who whoMatcher
                        , gameValueMatches n gameValueMatcher
                        , matches enemyId enemyMatcher
                        ]
                    _ -> noMatch
                  Window.FailSkillTest who n -> do
                    let unhandled = case skillMatcher of
                          Matcher.WhileAttackingAnEnemy _ -> False
                          Matcher.WhileEvadingAnEnemy _ -> False
                          Matcher.WhileInvestigating _ -> False
                          _ -> True
                    if unhandled
                      then
                        andM
                          [ matchWho iid who whoMatcher
                          , gameValueMatches n gameValueMatcher
                          ]
                      else noMatch
                  _ -> noMatch
                Matcher.SuccessResult gameValueMatcher -> guardTiming timing $ \case
                  Window.PassInvestigationSkillTest who lid n -> case skillMatcher of
                    Matcher.WhileInvestigating whereMatcher ->
                      andM
                        [ matchWho iid who whoMatcher
                        , gameValueMatches n gameValueMatcher
                        , locationMatches iid source window' lid whereMatcher
                        ]
                    _ -> noMatch
                  Window.SuccessfulAttackEnemy who _ enemyId n -> case skillMatcher of
                    Matcher.WhileAttackingAnEnemy enemyMatcher ->
                      andM
                        [ matchWho iid who whoMatcher
                        , gameValueMatches n gameValueMatcher
                        , matches enemyId enemyMatcher
                        ]
                    _ -> noMatch
                  Window.SuccessfulEvadeEnemy who enemyId n -> case skillMatcher of
                    Matcher.WhileEvadingAnEnemy enemyMatcher ->
                      andM
                        [ matchWho iid who whoMatcher
                        , gameValueMatches n gameValueMatcher
                        , matches enemyId enemyMatcher
                        ]
                    _ -> noMatch
                  Window.PassSkillTest _ _ who n -> do
                    let unhandled = case skillMatcher of
                          Matcher.WhileAttackingAnEnemy _ -> False
                          Matcher.WhileEvadingAnEnemy _ -> False
                          Matcher.WhileInvestigating _ -> False
                          _ -> True
                    if unhandled
                      then
                        andM
                          [ matchWho iid who whoMatcher
                          , gameValueMatches n gameValueMatcher
                          ]
                      else noMatch
                  _ -> noMatch
                Matcher.AnyResult -> guardTiming timing $ \case
                  Window.FailSkillTest who _ -> matchWho iid who whoMatcher
                  Window.PassSkillTest _ _ who _ -> matchWho iid who whoMatcher
                  _ -> noMatch
            isWindowMatch skillTestResultMatcher
    Matcher.DuringTurn whoMatcher -> guardTiming #when $ \case
      Window.NonFast -> matchWho iid iid whoMatcher
      Window.DuringTurn who -> matchWho iid who whoMatcher
      Window.FastPlayerWindow -> do
        miid <- selectOne Matcher.TurnInvestigator
        case miid of
          Nothing -> pure False
          Just who -> matchWho iid who whoMatcher
      _ -> noMatch
    Matcher.OrWindowMatcher matchers ->
      anyM (windowMatches iid rawSource window') matchers
    Matcher.TreacheryEntersPlay timing treacheryMatcher -> guardTiming timing $ \case
      Window.TreacheryEntersPlay treacheryId -> treacheryId <=~> treacheryMatcher
      _ -> noMatch
    Matcher.EnemySpawns timing whereMatcher enemyMatcher ->
      guardTiming timing $ \case
        Window.EnemySpawns enemyId locationId ->
          andM
            [ matches enemyId enemyMatcher
            , locationMatches iid source window' locationId whereMatcher
            ]
        _ -> noMatch
    Matcher.EnemyWouldAttack timing whoMatcher enemyAttackMatcher enemyMatcher ->
      guardTiming timing $ \case
        Window.EnemyWouldAttack details -> case attackTarget details of
          SingleAttackTarget (InvestigatorTarget who) ->
            andM
              [ matchWho iid who whoMatcher
              , matches (attackEnemy details) enemyMatcher
              , enemyAttackMatches iid details enemyAttackMatcher
              ]
          MassiveAttackTargets (mapMaybe (preview _InvestigatorTarget) -> iids) ->
            andM
              [ anyM (\who -> matchWho iid who whoMatcher) iids
              , matches (attackEnemy details) enemyMatcher
              , enemyAttackMatches iid details enemyAttackMatcher
              ]
          _ -> noMatch
        _ -> noMatch
    Matcher.EnemyAttacks timing whoMatcher enemyAttackMatcher enemyMatcher ->
      guardTiming timing $ \case
        Window.EnemyAttacks details -> case attackTarget details of
          SingleAttackTarget (InvestigatorTarget who) ->
            andM
              [ matchWho iid who whoMatcher
              , matches (attackEnemy details) enemyMatcher
              , enemyAttackMatches iid details enemyAttackMatcher
              ]
          MassiveAttackTargets (mapMaybe (preview _InvestigatorTarget) -> iids) ->
            andM
              [ anyM (\who -> matchWho iid who whoMatcher) iids
              , matches (attackEnemy details) enemyMatcher
              , enemyAttackMatches iid details enemyAttackMatcher
              ]
          _ -> noMatch
        _ -> noMatch
    Matcher.EnemyAttacksEvenIfCancelled timing whoMatcher enemyAttackMatcher enemyMatcher ->
      guardTiming timing $ \case
        Window.EnemyAttacksEvenIfCancelled details -> case attackTarget details of
          SingleAttackTarget (InvestigatorTarget who) ->
            andM
              [ matchWho iid who whoMatcher
              , matches (attackEnemy details) enemyMatcher
              , enemyAttackMatches iid details enemyAttackMatcher
              ]
          MassiveAttackTargets (mapMaybe (preview _InvestigatorTarget) -> iids) ->
            andM
              [ anyM (\who -> matchWho iid who whoMatcher) iids
              , matches (attackEnemy details) enemyMatcher
              , enemyAttackMatches iid details enemyAttackMatcher
              ]
          _ -> noMatch
        _ -> noMatch
    Matcher.EnemyAttacked timing whoMatcher sourceMatcher enemyMatcher ->
      guardTiming timing $ \case
        Window.EnemyAttacked who source' enemyId ->
          andM
            [ matchWho iid who whoMatcher
            , matches enemyId enemyMatcher
            , sourceMatches source' sourceMatcher
            ]
        _ -> noMatch
    Matcher.EnemyAttackedSuccessfully timing whoMatcher sourceMatcher enemyMatcher ->
      guardTiming timing $ \case
        Window.SuccessfulAttackEnemy who source' enemyId _ -> do
          andM
            [ matchWho iid who whoMatcher
            , matches enemyId enemyMatcher
            , sourceMatches source' sourceMatcher
            ]
        _ -> noMatch
    Matcher.AttemptToEvade timing whoMatcher enemyMatcher ->
      guardTiming timing $ \case
        Window.AttemptToEvadeEnemy _ who enemyId ->
          andM
            [ matchWho iid who whoMatcher
            , matches enemyId enemyMatcher
            ]
        _ -> noMatch
    Matcher.EnemyEvaded timing whoMatcher enemyMatcher ->
      guardTiming timing $ \case
        Window.EnemyEvaded who enemyId -> do
          -- we need to check defeated because things like Kymani's ability can discard them
          andM
            [ matchWho iid who whoMatcher
            , orM
                [ pure $ enemyMatcher == Matcher.AnyEnemy
                , matches enemyId enemyMatcher
                , matches enemyId (Matcher.OutOfPlayEnemy RemovedZone enemyMatcher)
                ]
            ]
        _ -> noMatch
    Matcher.EnemyEngaged timing whoMatcher enemyMatcher ->
      guardTiming timing $ \case
        Window.EnemyEngaged who enemyId ->
          andM
            [ matchWho iid who whoMatcher
            , matches enemyId enemyMatcher
            ]
        _ -> noMatch
    Matcher.MythosStep mythosStepMatcher -> guardTiming #when $ \case
      Window.AllDrawEncounterCard ->
        pure $ mythosStepMatcher == Matcher.WhenAllDrawEncounterCard
      Window.AfterCheckDoomThreshold ->
        pure $ mythosStepMatcher == Matcher.AfterCheckDoomThreshold
      _ -> noMatch
    Matcher.WouldRevealChaosToken timing whoMatcher -> guardTiming timing $ \case
      Window.WouldRevealChaosToken _ who -> matchWho iid who whoMatcher
      _ -> noMatch
    Matcher.WouldRevealChaosTokens timing whoMatcher -> guardTiming timing $ \case
      Window.WouldRevealChaosTokens _ who -> matchWho iid who whoMatcher
      _ -> noMatch
    Matcher.RevealChaosToken timing whoMatcher tokenMatcher -> guardTiming timing $ \case
      Window.RevealChaosToken who token ->
        andM
          [ matchWho iid who whoMatcher
          , matchChaosToken who token (IncludeSealed tokenMatcher)
          ]
      _ -> noMatch
    Matcher.ResolvesTreachery timing whoMatcher treacheryMatcher -> guardTiming timing $ \case
      Window.ResolvesTreachery who treacheryId ->
        andM [matchWho iid who whoMatcher, treacheryId <=~> IncludeOutOfPlayTreachery treacheryMatcher]
      _ -> noMatch
    Matcher.ResolvesChaosToken timing whoMatcher tokenMatcher -> guardTiming timing $ \case
      Window.ResolvesChaosToken who token ->
        andM [matchWho iid who whoMatcher, matchChaosToken who token tokenMatcher]
      _ -> noMatch
    Matcher.CancelChaosToken timing whoMatcher tokenMatcher ->
      guardTiming timing $ \case
        Window.CancelChaosToken who token ->
          andM [matchWho iid who whoMatcher, matchChaosToken who token tokenMatcher]
        _ -> noMatch
    Matcher.IgnoreChaosToken timing whoMatcher tokenMatcher ->
      guardTiming timing $ \case
        Window.IgnoreChaosToken who token ->
          andM [matchWho iid who whoMatcher, matchChaosToken who token tokenMatcher]
        _ -> noMatch
    Matcher.ChaosTokenSealed timing whoMatcher tokenMatcher ->
      guardTiming timing $ \case
        Window.ChaosTokenSealed who token ->
          andM [matchWho iid who whoMatcher, matchChaosToken who token tokenMatcher]
        _ -> noMatch
    Matcher.AddedToVictory timing cardMatcher -> guardTiming timing $ \case
      Window.AddedToVictory card -> pure $ cardMatch card cardMatcher
      _ -> noMatch
    Matcher.AssetDefeated timing defeatedByMatcher assetMatcher ->
      guardTiming timing $ \case
        Window.AssetDefeated assetId defeatedBy ->
          andM
            [ elem assetId <$> select assetMatcher
            , defeatedByMatches defeatedBy defeatedByMatcher
            ]
        _ -> noMatch
    Matcher.EnemyDefeated timing whoMatcher defeatedByMatcher enemyMatcher ->
      guardTiming timing $ \case
        Window.EnemyDefeated (Just who) defeatedBy enemyId ->
          andM
            [ matchWho iid who whoMatcher
            , matches enemyId $ if timing == #after then DefeatedEnemy enemyMatcher else enemyMatcher
            , defeatedByMatches defeatedBy defeatedByMatcher
            ]
        Window.EnemyDefeated Nothing defeatedBy enemyId | whoMatcher == Matcher.You -> do
          andM
            [ matches enemyId $ if timing == #after then DefeatedEnemy enemyMatcher else enemyMatcher
            , defeatedByMatches
                defeatedBy
                (defeatedByMatcher <> Matcher.BySource (Matcher.SourceOwnedBy $ Matcher.InvestigatorWithId iid))
            ]
        Window.EnemyDefeated Nothing defeatedBy enemyId | whoMatcher == Matcher.Anyone -> do
          andM
            [ matches enemyId $ if timing == #after then DefeatedEnemy enemyMatcher else enemyMatcher
            , defeatedByMatches defeatedBy defeatedByMatcher
            ]
        _ -> noMatch
    Matcher.EnemyEnters timing whereMatcher enemyMatcher ->
      guardTiming timing $ \case
        Window.EnemyEnters enemyId lid ->
          andM
            [ matches enemyId enemyMatcher
            , locationMatches iid source window' lid whereMatcher
            ]
        _ -> noMatch
    Matcher.EnemyLeaves timing whereMatcher enemyMatcher ->
      guardTiming timing $ \case
        Window.EnemyLeaves enemyId lid ->
          andM
            [ matches enemyId enemyMatcher
            , locationMatches iid source window' lid whereMatcher
            ]
        _ -> noMatch
    Matcher.ChosenRandomLocation timing whereMatcher -> guardTiming timing $ \case
      Window.ChosenRandomLocation lid -> locationMatches iid source window' lid whereMatcher
      _ -> noMatch
    Matcher.EnemyWouldBeDefeated timing enemyMatcher -> guardTiming timing $ \case
      Window.EnemyWouldBeDefeated enemyId -> matches enemyId enemyMatcher
      _ -> noMatch
    Matcher.EnemyWouldReady timing enemyMatcher -> guardTiming timing $ \case
      Window.WouldReady (EnemyTarget enemyId) -> matches enemyId enemyMatcher
      _ -> noMatch
    Matcher.EnemyReadies timing enemyMatcher -> guardTiming timing $ \case
      Window.Readies (EnemyTarget enemyId) -> matches enemyId enemyMatcher
      _ -> noMatch
    Matcher.FastPlayerWindow -> guardTiming #when (pure . (== Window.FastPlayerWindow))
    Matcher.DealtDamageOrHorror timing sourceMatcher whoMatcher -> guardTiming timing $ \case
      Window.WouldTakeDamageOrHorror source' (InvestigatorTarget iid') _ _ ->
        andM [matchWho iid iid' whoMatcher, sourceMatches source' sourceMatcher]
      Window.WouldTakeDamageOrHorror source' (AssetTarget aid) _ _ ->
        andM
          [ elem aid
              <$> select
                ( Matcher.AssetControlledBy
                    $ Matcher.replaceYouMatcher iid whoMatcher
                )
          , sourceMatches source' sourceMatcher
          ]
      _ -> noMatch
    Matcher.DealtDamage timing sourceMatcher whoMatcher -> guardTiming timing $ \case
      Window.DealtDamage source' _ (InvestigatorTarget iid') _ ->
        andM [matchWho iid iid' whoMatcher, sourceMatches source' sourceMatcher]
      Window.DealtDamage source' _ (AssetTarget aid) _ ->
        andM
          [ elem aid
              <$> select
                (Matcher.AssetControlledBy $ Matcher.replaceYouMatcher iid whoMatcher)
          , sourceMatches source' sourceMatcher
          ]
      _ -> noMatch
    Matcher.DealtHorror timing sourceMatcher whoMatcher -> guardTiming timing $ \case
      Window.DealtHorror source' (InvestigatorTarget iid') _ ->
        andM [matchWho iid iid' whoMatcher, sourceMatches source' sourceMatcher]
      Window.DealtHorror source' (AssetTarget aid) _ ->
        andM
          [ elem aid
              <$> select
                (Matcher.AssetControlledBy $ Matcher.replaceYouMatcher iid whoMatcher)
          , sourceMatches source' sourceMatcher
          ]
      _ -> noMatch
    Matcher.AssignedHorror timing whoMatcher targetListMatcher ->
      guardTiming timing $ \case
        Window.AssignedHorror _ who targets ->
          andM
            [ matchWho iid who whoMatcher
            , targetListMatches targets targetListMatcher
            ]
        _ -> noMatch
    Matcher.AssetDealtDamage timing sourceMatcher assetMatcher ->
      guardTiming timing $ \case
        Window.DealtDamage source' _ (AssetTarget aid) _ ->
          andM
            [ elem aid <$> select assetMatcher
            , sourceMatches source' sourceMatcher
            ]
        _ -> noMatch
    Matcher.AssetDealtDamageOrHorror timing sourceMatcher assetMatcher ->
      guardTiming timing $ \case
        Window.DealtDamage source' _ (AssetTarget aid) _ ->
          andM
            [ elem aid <$> select assetMatcher
            , sourceMatches source' sourceMatcher
            ]
        Window.DealtHorror source' (AssetTarget aid) _ ->
          andM
            [ elem aid <$> select assetMatcher
            , sourceMatches source' sourceMatcher
            ]
        _ -> noMatch
    Matcher.EnemyDealtDamage timing damageEffectMatcher enemyMatcher sourceMatcher ->
      guardTiming timing $ \case
        Window.DealtDamage source' damageEffect (EnemyTarget eid) _ ->
          andM
            [ damageEffectMatches damageEffect damageEffectMatcher
            , elem eid <$> select enemyMatcher
            , sourceMatches source' sourceMatcher
            ]
        _ -> noMatch
    Matcher.EnemyDealtExcessDamage timing damageEffectMatcher enemyMatcher sourceMatcher ->
      guardTiming timing $ \case
        Window.DealtExcessDamage source' damageEffect (EnemyTarget eid) _ ->
          andM
            [ damageEffectMatches damageEffect damageEffectMatcher
            , elem eid <$> select enemyMatcher
            , sourceMatches source' sourceMatcher
            ]
        _ -> noMatch
    Matcher.EnemyTakeDamage timing damageEffectMatcher enemyMatcher valueMatcher sourceMatcher ->
      guardTiming timing $ \case
        Window.TakeDamage source' damageEffect (EnemyTarget eid) n ->
          andM
            [ damageEffectMatches damageEffect damageEffectMatcher
            , elem eid <$> select enemyMatcher
            , sourceMatches source' sourceMatcher
            , gameValueMatches n valueMatcher
            ]
        _ -> noMatch
    Matcher.SpentClues timing whoMatcher valueMatcher -> guardTiming timing $ \case
      Window.SpentClues who n ->
        andM
          [ matchWho iid who whoMatcher
          , gameValueMatches n valueMatcher
          ]
      _ -> noMatch
    Matcher.DiscoverClues timing whoMatcher whereMatcher valueMatcher ->
      guardTiming timing $ \case
        Window.DiscoverClues who lid _ n ->
          andM
            [ matchWho iid who (Matcher.replaceThatLocation lid whoMatcher)
            , locationMatches iid source window' lid whereMatcher
            , gameValueMatches n valueMatcher
            ]
        _ -> noMatch
    Matcher.WouldDiscoverClues timing whoMatcher whereMatcher valueMatcher ->
      guardTiming timing $ \case
        Window.WouldDiscoverClues who lid _ n ->
          andM
            [ matchWho iid who (Matcher.replaceThatLocation lid whoMatcher)
            , locationMatches iid source window' lid whereMatcher
            , gameValueMatches n valueMatcher
            ]
        _ -> noMatch
    Matcher.GainsClues timing whoMatcher valueMatcher -> guardTiming timing $ \case
      Window.GainsClues who _ n ->
        andM [matchWho iid who whoMatcher, gameValueMatches n valueMatcher]
      _ -> noMatch
    Matcher.GainsResources timing whoMatcher sourceMatcher valueMatcher -> guardTiming timing $ \case
      Window.GainsResources who source' n ->
        andM
          [ matchWho iid who whoMatcher
          , gameValueMatches n valueMatcher
          , sourceMatches source' sourceMatcher
          ]
      _ -> noMatch
    Matcher.DiscoveringLastClue timing whoMatcher whereMatcher ->
      guardTiming timing $ \case
        Window.DiscoveringLastClue who lid ->
          andM
            [ matchWho iid who whoMatcher
            , locationMatches iid source window' lid whereMatcher
            ]
        _ -> noMatch
    Matcher.LastClueRemovedFromAsset timing assetMatcher -> guardTiming timing $ \case
      Window.LastClueRemovedFromAsset aid -> elem aid <$> select assetMatcher
      _ -> noMatch
    Matcher.DrawsCards timing whoMatcher valueMatcher -> guardTiming timing $ \case
      Window.DrawCards who cards ->
        andM
          [ matchWho iid who whoMatcher
          , gameValueMatches (length cards) valueMatcher
          ]
      _ -> noMatch
    Matcher.DrawCard timing whoMatcher cardMatcher deckMatcher ->
      guardTiming timing $ \case
        Window.DrawCard who card deck ->
          andM
            [ matchWho iid who whoMatcher
            , case cardMatcher of
                Matcher.BasicCardMatch baseMatcher ->
                  pure $ cardMatch card baseMatcher
                _ -> elem card <$> select cardMatcher
            , deckMatch iid deck deckMatcher
            ]
        _ -> noMatch
    Matcher.WouldDrawCard timing whoMatcher deckMatcher ->
      guardTiming timing $ \case
        Window.WouldDrawCard who deck ->
          andM
            [ matchWho iid who whoMatcher
            , deckMatch iid deck $ Matcher.replaceThatInvestigator who deckMatcher
            ]
        _ -> noMatch
    Matcher.ResolvingRevelation timing whoMatcher treacheryMatcher -> guardTiming timing \case
      Window.ResolvingRevelation who treachery ->
        andM
          [ matchWho iid who whoMatcher
          , treachery <=~> treacheryMatcher
          ]
      _ -> noMatch
    Matcher.DeckWouldRunOutOfCards timing whoMatcher -> guardTiming timing $ \case
      Window.DeckWouldRunOutOfCards who -> matchWho iid who whoMatcher
      _ -> noMatch
    Matcher.DeckHasNoCards timing whoMatcher -> guardTiming timing $ \case
      Window.DeckHasNoCards who -> matchWho iid who whoMatcher
      _ -> noMatch
    Matcher.EncounterDeckRunsOutOfCards -> pure $ wType == Window.EncounterDeckRunsOutOfCards
    Matcher.PlayCard timing whoMatcher cardMatcher -> guardTiming timing $ \case
      Window.PlayCard who cardPlay ->
        andM
          [ matchWho iid who whoMatcher
          , case cardMatcher of
              Matcher.BasicCardMatch baseMatcher ->
                pure $ cardMatch cardPlay.card baseMatcher
              _ ->
                elem cardPlay.card <$> select (Matcher.basic (Matcher.CardWithId cardPlay.card.id) <> cardMatcher)
          ]
      _ -> noMatch
    Matcher.PlayEventDiscarding timing whoMatcher eventMatcher -> guardTiming timing $ \case
      Window.PlayEventDiscarding who event ->
        andM
          [ matchWho iid who whoMatcher
          , event <=~> eventMatcher
          ]
      _ -> noMatch
    Matcher.PlayEvent timing whoMatcher eventMatcher -> guardTiming timing $ \case
      Window.PlayEvent who event ->
        andM
          [ matchWho iid who whoMatcher
          , case eventMatcher of
              EventWithId eid -> pure $ event == eid
              _ -> event <=~> OutOfPlayEvent eventMatcher
          ]
      _ -> noMatch
    Matcher.AgendaEntersPlay timing agendaMatcher -> guardTiming timing $ \case
      Window.EnterPlay (AgendaTarget aid) -> elem aid <$> select agendaMatcher
      _ -> noMatch
    Matcher.AssetEntersPlay timing assetMatcher -> guardTiming timing $ \case
      Window.EnterPlay (AssetTarget aid) -> elem aid <$> select assetMatcher
      _ -> noMatch
    Matcher.AssetLeavesPlay timing assetMatcher -> guardTiming timing $ \case
      Window.LeavePlay (AssetTarget aid) -> elem aid <$> select assetMatcher
      _ -> noMatch
    Matcher.AssetDiscarded timing assetMatcher -> guardTiming timing $ \case
      Window.LeavePlay (AssetTarget aid) -> elem aid <$> select assetMatcher
      _ -> noMatch
    Matcher.EnemyEntersPlay timing enemyMatcher -> guardTiming timing $ \case
      Window.EnterPlay (EnemyTarget eid) -> matches eid enemyMatcher
      Window.EnemySpawns eid _ -> matches eid enemyMatcher
      _ -> noMatch
    Matcher.LocationLeavesPlay timing locationMatcher -> guardTiming timing $ \case
      Window.LeavePlay (LocationTarget aid) -> elem aid <$> select locationMatcher
      _ -> noMatch
    Matcher.EnemyLeavesPlay timing enemyMatcher -> guardTiming timing $ \case
      Window.LeavePlay (EnemyTarget eid) -> elem eid <$> select enemyMatcher
      _ -> noMatch
    Matcher.Explored timing whoMatcher resultMatcher -> guardTiming timing $ \case
      Window.Explored who result ->
        andM
          [ matchWho iid who whoMatcher
          , case resultMatcher of
              Matcher.SuccessfulExplore locationMatcher -> case result of
                Window.Success lid -> lid <=~> locationMatcher
                Window.Failure _ -> noMatch
              Matcher.FailedExplore cardMatcher -> case result of
                Window.Success _ -> noMatch
                Window.Failure card -> pure $ cardMatch card cardMatcher
          ]
      _ -> noMatch
    Matcher.AttemptExplore timing whoMatcher -> guardTiming timing $ \case
      Window.AttemptExplore who -> matchWho iid who whoMatcher
      _ -> noMatch
