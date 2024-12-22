module Arkham.Helpers.Window where

import Arkham.Prelude

import Arkham.Attack.Types
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillTest.Base (SkillTest)
import Arkham.Source (Source)
import Arkham.Target
import Arkham.Timing (Timing)
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Window
import Arkham.Window qualified as Window

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
    _ -> Nothing

discoveredLocation :: HasCallStack => [Window] -> LocationId
discoveredLocation =
  fromMaybe (error "missing discovery") . asum . map \case
    (windowType -> Window.DiscoverClues _ lid _ _) -> Just lid
    _ -> Nothing

locationLeavingPlay :: HasCallStack => [Window] -> LocationId
locationLeavingPlay =
  fromMaybe (error "missing locationLeavingPlay") . asum . map \case
    (windowType -> Window.LeavePlay (LocationTarget lid)) -> Just lid
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

cardDrawn :: HasCallStack => [Window] -> Card
cardDrawn [] = error "missing play card window"
cardDrawn ((windowType -> Window.DrawCard _ c _) : _) = c
cardDrawn (_ : xs) = cardDrawn xs

cardDrawnBy :: HasCallStack => [Window] -> (InvestigatorId, Card)
cardDrawnBy [] = error "missing play card window"
cardDrawnBy ((windowType -> Window.DrawCard iid c _) : _) = (iid, c)
cardDrawnBy (_ : xs) = cardDrawnBy xs

cardsDrawn :: [Window] -> [Card]
cardsDrawn [] = []
cardsDrawn ((windowType -> Window.DrawCards _ cs) : rest) = cs <> cardsDrawn rest
cardsDrawn (_ : xs) = cardsDrawn xs

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
    _ -> Nothing

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
  (_ : rest) -> getPassedBy rest

getDamageSource :: HasCallStack => [Window] -> Source
getDamageSource = \case
  [] -> error "No damage"
  ((windowType -> Window.DealtDamage source _ _ _) : _) -> source
  ((windowType -> Window.DealtExcessDamage source _ _ _) : _) -> source
  (_ : rest) -> getDamageSource rest

getDamageOrHorrorSource :: HasCallStack => [Window] -> Source
getDamageOrHorrorSource = \case
  [] -> error "No damage"
  ((windowType -> Window.DealtDamage source _ _ _) : _) -> source
  ((windowType -> Window.DealtHorror source _ _) : _) -> source
  ((windowType -> Window.DealtExcessDamage source _ _ _) : _) -> source
  (_ : rest) -> getDamageSource rest

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
