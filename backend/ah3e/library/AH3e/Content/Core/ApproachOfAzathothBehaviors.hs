module AH3e.Content.Core.ApproachOfAzathothBehaviors (behaviors) where

import AH3e.Engine.Behavior
import AH3e.Engine.Helpers
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill
import AH3e.Types.State
import Data.Map.Strict qualified as Map

behaviors :: Behaviors
behaviors =
  mempty
    { codex =
        Map.fromList
          [ (3, dejaVu)
          , (4, profaneRitual)
          , (5, timeUnstuck)
          , (6, ritualSite)
          , (7, strangeBarrier)
          , (8, azathothAwakens)
          , (9, theFuture)
          ]
    , customEffects =
        Map.fromList
          [ ("aoa-reckoning", scenarioReckoning)
          , ("aoa-future-ritual", futureRitual)
          ]
    }

cultist :: Trait
cultist = "Cultist"

theFutureSpace :: SpaceId
theFutureSpace = "the-future"

entryOf :: ArchiveNumber -> GameM (Maybe CodexEntry)
entryOf n = uses #codex (listToMaybe . filter ((== n) . (.number)))

inCodex :: ArchiveNumber -> GameM Bool
inCodex n = isJust <$> entryOf n

barrierDestroyed :: GameM Bool
barrierDestroyed = maybe False (.flipped) <$> entryOf 6

doomTrigger :: Text -> Int -> (CodexEntry -> Bool) -> [Message] -> CodexTrigger
doomTrigger key n side msgs =
  CodexTrigger
    { key = key
    , once = True
    , condition = \e -> do
        doom <- use #sheetDoom
        pure (side e && doom >= n)
    , action = \_ -> pushAll msgs
    }

atMarker :: Text -> InvestigatorId -> GameM Bool
atMarker color iid = do
  msid <- investigatorSpace iid
  site <- markerSpace color
  pure (isJust msid && msid == site)

spendSheetClues :: Int -> GameM ()
spendSheetClues n = #sheetClues %= max 0 . subtract n

setMarkers :: SpaceId -> ([Marker] -> [Marker]) -> GameM ()
setMarkers sid f = spaceL sid . #markers %= f

-- Scenario sheet
scenarioReckoning :: EffectCtx -> GameM ()
scenarioReckoning _ = do
  ms <- uses #monsters Map.elems
  cultists <- filterM (\m -> elem cultist . (.traits) <$> monsterDef m.card) ms
  push (PlaceDoomInOrder SourceScenario (map (.space) cultists))

spawnCultistsAt :: Int -> [SpaceId] -> GameM ()
spawnCultistsAt n spaces = do
  found <- revealMonstersFromBottom cultist n
  for_ found \mid ->
    chooseGroup "Choose where the cultist spawns" (spaceChoices spaces \s -> [PlaceMonster mid s Ready])

-- 3
dejaVu :: CodexBehavior
dejaVu =
  defaultCodexBehavior
    { triggers =
        [ CodexTrigger
            { key = "objective"
            , once = True
            , condition = \e -> do
                clues <- use #sheetClues
                four <- inCodex 4
                pure (not e.flipped && clues >= 3 && not four)
            , action = \_ -> push (AddArchiveToCodex 4)
            }
        , doomTrigger "flip" 3 (not . (.flipped)) [FlipCodexCard 3]
        ]
    , onFlip = \e -> when e.flipped do
        l <- leaderPlayer
        leaderSpace <- investigatorOfPlayer l >>= maybe (pure Nothing) investigatorSpace
        streets <- maybe (pure []) (nearestSpacesMatching isStreetLike) leaderSpace
        unless (null streets) $ spawnCultistsAt 1 streets
        pushAll [AddArchiveToCodex 5, RemoveCodexCard 3]
    }

-- 4
profaneRitual :: CodexBehavior
profaneRitual =
  defaultCodexBehavior
    { onAdd = \e -> unless e.flipped do
        colors <- shuffle ["blue", "red", "white", "white", "white"]
        for_
          ( zip
              colors
              ( map
                  spaceIdFor'
                  ["arkham-advertiser", "black-cave", "independence-square", "unvisited-isle", "velmas-diner"]
              )
          )
          \(c, sid) ->
            setMarkers sid (<> [Marker c False])
        push (FlipCodexCard 4)
    , componentActions =
        [ ComponentActionDef
            { label = "Profane Ritual"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                me <- entryOf 4
                msid <- investigatorSpace iid
                hidden <- maybe (pure []) (fmap (filter (not . (.faceUp))) . markersAt) msid
                pure (maybe False (.flipped) me && not (null hidden))
            , perform = revealMarker
            }
        ]
    }

revealMarker :: EffectCtx -> GameM ()
revealMarker ctx = do
  sid <- fromJustNote "space" <$> investigatorSpace ctx.investigator
  ms <- markersAt sid
  case break (not . (.faceUp)) ms of
    (before, m : after) -> do
      setMarkers sid (const (before <> [m {faceUp = True}] <> after))
      logText ("Revealed a " <> m.color <> " marker")
      case m.color of
        "white" -> setMarkers sid (filter (/= m {faceUp = True}))
        "blue" -> do
          noteRevealed "blue"
          push (AddArchiveToCodex 6)
        "red" -> do
          noteRevealed "red"
          destroyed <- barrierDestroyed
          push (if destroyed then AddArchiveToCodexFlipped 7 else AddArchiveToCodex 7)
        _ -> pure ()
      me <- entryOf 4
      for_ me \e -> when (Map.member "blue" e.tokens && Map.member "red" e.tokens) do
        spaces <- uses (#board . #spaces) Map.keys
        for_ spaces \s -> setMarkers s (filter ((`elem` ["blue", "red"]) . (.color)))
        pushEnd (RemoveCodexCard 4)
    _ -> pure ()
 where
  noteRevealed color = #codex %= map (\e -> if e.number == 4 then e & #tokens %~ Map.insert color 1 else e)

-- 5
timeUnstuck :: CodexBehavior
timeUnstuck =
  defaultCodexBehavior
    { triggers =
        [ CodexTrigger
            { key = "objective"
            , once = False
            , condition = \e -> do
                clues <- use #sheetClues
                markers <- allMarkers
                four <- inCodex 4
                pure (not e.flipped && clues >= 3 && null markers && not four)
            , action = \_ -> push (AddArchiveToCodex 4)
            }
        , doomTrigger
            "time"
            7
            (not . (.flipped))
            [AddArchiveToCodex 8, AddArchiveToCodex 9, RemoveCodexCard 5]
        ]
    }

-- 6
ritualSite :: CodexBehavior
ritualSite =
  defaultCodexBehavior
    { onAdd = \e -> unless e.flipped do
        site <- markerSpace "blue"
        for_ site \s -> spawnCultistsAt 2 [s]
    , componentActions =
        [ ComponentActionDef
            { label = "Stop the ritual"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                me <- entryOf 6
                here <- atMarker "blue" iid
                clues <- use #sheetClues
                pure (maybe False (not . (.flipped)) me && here && clues >= 5)
            , perform = \_ -> do
                spendSheetClues 5
                site <- markerSpace "blue"
                for_ site \s -> setMarkers s (filter ((/= "blue") . (.color)))
                push (FlipCodexCard 6)
            }
        ]
    }

-- 7
strangeBarrier :: CodexBehavior
strangeBarrier =
  defaultCodexBehavior
    { onAdd = \e -> unless e.flipped do
        epicenter <- markerSpace "red"
        board <- use #board
        for_ epicenter \ep -> do
          here <- investigatorsAt ep
          for_ here \i ->
            chooseFor i.id "Move one space away from the epicenter"
              $ spaceChoices (filter (/= ep) (adjacentSpaces ep board)) \s -> [MoveDirectly i.id s]
    , blockedSpaces = \e -> if e.flipped then pure [] else maybeToList <$> markerSpace "red"
    , triggers =
        [ CodexTrigger
            { key = "barrier"
            , once = True
            , condition = \e -> (not e.flipped &&) <$> barrierDestroyed
            , action = \_ -> push (FlipCodexCard 7)
            }
        ]
    , componentActions =
        [ ComponentActionDef
            { label = "Erect a protective ward over Arkham"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                me <- entryOf 7
                here <- atMarker "red" iid
                clues <- use #sheetClues
                pure (maybe False (.flipped) me && here && clues >= 3)
            , perform = \_ -> do
                spendSheetClues 3
                pushAll [LogText "Time Unstuck: Investigators win the game!", WinTheGame]
            }
        ]
    }

-- 8
azathothAwakens :: CodexBehavior
azathothAwakens =
  defaultCodexBehavior
    { onAdd = \e -> unless e.flipped do
        #cup %= (<> [SpreadDoomToken, GateBurstToken])
        spaces <- uses (#board . #spaces) Map.keys
        for_ spaces \s -> setMarkers s (map (\m -> m {faceUp = True}) . filter ((== "red") . (.color)))
        markers <- allMarkers
        when (null markers) do
          start <- (.startingSpace) <$> getScenarioDef
          setMarkers start (<> [Marker "red" True])
        pushAll [RemoveCodexCard 4, RemoveCodexCard 6, RemoveCodexCard 7, FlipCodexCard 8]
    , componentActions =
        [ ComponentActionDef
            { label = "Move to the future"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                me <- entryOf 8
                here <- atMarker "red" iid
                future <- uses (#board . #spaces) (Map.member theFutureSpace)
                pure (maybe False (.flipped) me && here && future)
            , perform = \ctx -> push (MoveDirectly ctx.investigator theFutureSpace)
            }
        ]
    , triggers =
        [ CodexTrigger
            { key = "lost-in-time"
            , once = True
            , condition = \e -> pure (e.flipped && Map.findWithDefault 0 "clues" e.tokens >= 5)
            , action = \_ -> pushAll [FlipCodexCard 9, LogText "Lost in Time: Investigators win the game!", WinTheGame]
            }
        , doomTrigger
            "blind-chaos"
            13
            (.flipped)
            [FlipCodexCard 9, LogText "Blind Chaos: Investigators lose the game.", LoseTheGame "Blind Chaos"]
        ]
    }

-- 9
theFuture :: CodexBehavior
theFuture =
  defaultCodexBehavior
    { onAdd = \_ ->
        #board
          . #spaces
          . at theFutureSpace
          ?= Space
            { id = theFutureSpace
            , name = "The Future"
            , kind = SpecialSpace
            , neighborhood = Nothing
            , doom = 0
            , clues = 0
            , markers = []
            }
    , componentActions =
        [ ComponentActionDef
            { label = "Prepare the Elder Sign ritual"
            , allowedWhileEngaged = False
            , canPerform = \iid -> (== Just theFutureSpace) <$> investigatorSpace iid
            , perform = \ctx ->
                push
                  ( BeginTest
                      ( newTest
                          ctx.investigator
                          Lore
                          (-2)
                          (ActionTest (ComponentAction (CodexRef 9) 0) Nothing)
                          (AfterEffect ctx (Custom "aoa-future-ritual") NoEffect)
                      )
                  )
            }
        ]
    , spaceEncounter = \_ sid -> if sid == theFutureSpace then Just (SufferHorror (N 2)) else Nothing
    }

futureRitual :: EffectCtx -> GameM ()
futureRitual _ = do
  clues <- use #sheetClues
  when (clues >= 1) do
    #sheetClues -= 1
    #codex %= map (\e -> if e.number == 8 then e & #tokens %~ Map.insertWith (+) "clues" 1 else e)
    push CheckStateTriggers

spaceIdFor' :: Text -> SpaceId
spaceIdFor' = SpaceId
