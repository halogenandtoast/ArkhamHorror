module Arkham.Helpers.Window where

import Arkham.Prelude

import Arkham.Attack.Types
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillTest.Base (SkillTest)
import Arkham.Source (Source)
import Arkham.Timing (Timing)
import Arkham.Timing qualified as Timing
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
  mBatchId <- getCurrentBatchId
  iids <- select UneliminatedInvestigator
  let windows'' = map (\w -> w {windowBatchId = windowBatchId w <|> mBatchId}) windows'
  if null iids
    then do
      iids' <- select Anyone
      pure $ CheckWindow iids' windows''
    else pure $ CheckWindow iids windows''

windows :: HasGame m => [WindowType] -> m [Message]
windows windows' = do
  iids <- select UneliminatedInvestigator
  pure $ do
    timing <- [Timing.When, Timing.AtIf, Timing.After]
    [CheckWindow iids $ map (mkWindow timing) windows']

wouldWindows :: (MonadRandom m, HasGame m) => WindowType -> m (BatchId, [Message])
wouldWindows window = do
  batchId <- getRandom
  iids <- select UneliminatedInvestigator
  pure
    ( batchId
    , [ CheckWindow iids [Window timing window (Just batchId)]
      | timing <- [Timing.When, Timing.AtIf, Timing.After]
      ]
    )

frame :: HasGame m => WindowType -> m (Message, Message, Message)
frame window = do
  iids <- select UneliminatedInvestigator
  let (whenWindow, atIfWindow, afterWindow) = timings window
  pure
    (CheckWindow iids [whenWindow], CheckWindow iids [atIfWindow], CheckWindow iids [afterWindow])

timings :: WindowType -> (Window, Window, Window)
timings wType = (mkWhen wType, mkAtIf wType, mkAfter wType)

batchedTimings :: BatchId -> WindowType -> (Window, Window, Window)
batchedTimings batchId wType = case timings wType of
  (whenWindow, atIfWindow, afterWindow) ->
    ( whenWindow {windowBatchId = Just batchId}
    , atIfWindow {windowBatchId = Just batchId}
    , afterWindow {windowBatchId = Just batchId}
    )

doFrame :: HasGame m => Message -> WindowType -> m [Message]
doFrame msg window = do
  (before, atIf, after) <- frame window
  pure [before, atIf, Do msg, after]

doBatch :: HasGame m => BatchId -> Message -> WindowType -> m [Message]
doBatch batchId msg window = do
  (before, atIf, after) <- frame window
  pure [before, atIf, DoBatch batchId msg, after]

pushBatch :: HasQueue Message m => BatchId -> Message -> m ()
pushBatch batchId msg = push $ Would batchId [msg]

pushBatched :: HasQueue Message m => BatchId -> [Message] -> m ()
pushBatched batchId msgs = push $ Would batchId msgs

wouldDo
  :: (MonadRandom m, HasGame m, HasQueue Message m) => Message -> WindowType -> WindowType -> m ()
wouldDo msg wouldWindow window = do
  (batchId, wouldWindowsMsgs) <- wouldWindows wouldWindow
  framed <- doBatch batchId msg window
  push $ Would batchId $ wouldWindowsMsgs <> framed

{- | Take a message which would operate on some value n and instead expand the
windows to add a single one at a time
-}
wouldDoEach
  :: (MonadRandom m, HasGame m, HasQueue Message m)
  => Int
  -> Message
  -> WindowType -- outer would window
  -> WindowType -- would window
  -> WindowType -- outer window
  -> WindowType -- window
  -> m ()
wouldDoEach n msg outerWouldWindow wouldWindow outerWindow window = do
  (outerBatchId, outerWouldWindowsMsgs) <- wouldWindows outerWouldWindow
  (outerBefore, outerAtIf, outerAfter) <- frame outerWindow
  frames <- replicateM n do
    (innerBatchId, innerWouldWindowsMsgs) <- wouldWindows wouldWindow
    framed <- doFrame msg window
    pure $ Would innerBatchId $ innerWouldWindowsMsgs <> framed

  push
    $ Would outerBatchId
    $ outerWouldWindowsMsgs
    <> [outerBefore, outerAtIf]
    <> frames
    <> [outerAfter]

splitWithWindows :: HasGame m => Message -> [WindowType] -> m [Message]
splitWithWindows msg windows' = do
  iids <- select UneliminatedInvestigator
  pure
    $ [CheckWindow iids $ map (mkWindow Timing.When) windows']
    <> [msg]
    <> [CheckWindow iids $ map (mkWindow Timing.After) windows']

discoveredClues :: [Window] -> Int
discoveredClues =
  fromMaybe (error "missing discovery") . asum . map \case
    (windowType -> Window.DiscoverClues _ _ _ n) -> Just n
    _ -> Nothing

discoveredLocation :: [Window] -> LocationId
discoveredLocation =
  fromMaybe (error "missing discovery") . asum . map \case
    (windowType -> Window.DiscoverClues _ lid _ _) -> Just lid
    _ -> Nothing

discoveredLocationAndClues :: [Window] -> (LocationId, Int)
discoveredLocationAndClues =
  fromMaybe (error "missing discovery") . asum . map \case
    (windowType -> Window.DiscoverClues _ lid _ n) -> Just (lid, n)
    _ -> Nothing

defeatedEnemy :: [Window] -> EnemyId
defeatedEnemy =
  fromMaybe (error "missing enemy") . asum . map \case
    (windowType -> Window.EnemyDefeated _ _ eid) -> Just eid
    _ -> Nothing

evadedEnemy :: [Window] -> EnemyId
evadedEnemy =
  fromMaybe (error "missing enemy") . asum . map \case
    (windowType -> Window.EnemyEvaded _ eid) -> Just eid
    _ -> Nothing

spawnedEnemy :: [Window] -> EnemyId
spawnedEnemy =
  fromMaybe (error "missing enemy") . asum . map \case
    (windowType -> Window.EnemySpawns eid _) -> Just eid
    _ -> Nothing

cardPlayed :: [Window] -> Card
cardPlayed [] = error "missing play card window"
cardPlayed ((windowType -> Window.PlayCard _ c) : _) = c
cardPlayed (_ : xs) = cardPlayed xs

cardDrawn :: [Window] -> Card
cardDrawn [] = error "missing play card window"
cardDrawn ((windowType -> Window.DrawCard _ c _) : _) = c
cardDrawn (_ : xs) = cardDrawn xs

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

getChaosToken :: [Window] -> ChaosToken
getChaosToken = \case
  [] -> error "No chaos token drawn"
  ((windowType -> Window.RevealChaosToken _ token) : _) -> token
  ((windowType -> Window.ResolvesChaosToken _ token) : _) -> token
  (_ : rest) -> getChaosToken rest

getAttackDetails :: [Window] -> EnemyAttackDetails
getAttackDetails = \case
  [] -> error "No attack details"
  ((windowType -> Window.EnemyWouldAttack details) : _) -> details
  ((windowType -> Window.EnemyAttacks details) : _) -> details
  ((windowType -> Window.EnemyAttacksEvenIfCancelled details) : _) -> details
  (_ : rest) -> getAttackDetails rest

getInvestigatedLocation :: [Window] -> LocationId
getInvestigatedLocation = \case
  [] -> error "No fail or pass skill test"
  ((windowType -> Window.FailInvestigationSkillTest _ lid _) : _) -> lid
  ((windowType -> Window.PassInvestigationSkillTest _ lid _) : _) -> lid
  (_ : rest) -> getInvestigatedLocation rest

getDamageSource :: [Window] -> Source
getDamageSource = \case
  [] -> error "No damage"
  ((windowType -> Window.DealtDamage source _ _ _) : _) -> source
  ((windowType -> Window.DealtExcessDamage source _ _ _) : _) -> source
  (_ : rest) -> getDamageSource rest

replaceWindow :: HasQueue Message m => (Window -> Bool) -> (Window -> Window) -> m ()
replaceWindow f wf = do
  replaceMessageMatching
    \case
      CheckWindow _ ws -> any f ws
      _ -> False
    \case
      CheckWindow iids ws -> [CheckWindow iids $ map (\w -> if f w then wf w else w) ws]
      _ -> error "impossible"

replaceWindowMany
  :: HasQueue Message m => (WindowType -> Bool) -> (WindowType -> [WindowType]) -> m ()
replaceWindowMany f wf = do
  replaceAllMessagesMatching
    \case
      CheckWindow _ ws -> any (f . windowType) ws
      RunWindow _ ws -> any (f . windowType) ws
      _ -> False
    \case
      CheckWindow iids ws ->
        [ CheckWindow iids
            $ concatMap
              (\w -> if f w.kind then map (`replaceWindowType` w) (wf w.kind) else [w])
              ws
        ]
      RunWindow iid ws ->
        [ RunWindow iid
            $ concatMap
              (\w -> if f w.kind then map (`replaceWindowType` w) (wf w.kind) else [w])
              ws
        ]
      _ -> error "impossible"

windowSkillTest :: [Window] -> Maybe SkillTest
windowSkillTest = \case
  [] -> Nothing
  ((windowType -> Window.InitiatedSkillTest st) : _) -> Just st
  (_ : rest) -> windowSkillTest rest
