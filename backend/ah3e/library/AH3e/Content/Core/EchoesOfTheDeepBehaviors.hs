-- | Mechanics for Echoes of the Deep.
module AH3e.Content.Core.EchoesOfTheDeepBehaviors (behaviors) where

import AH3e.Content.Tiles (spaceIdFor)
import AH3e.Engine.Behavior
import AH3e.Engine.Helpers
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill
import AH3e.Types.State
import Data.Map.Strict qualified as Map

behaviors :: Behaviors
behaviors =
  mempty
    & #codex
    .~ Map.fromList
      ( [ (29, theStarsAreRight)
        , (30, echoesOfTheDeep)
        , (31, lostCityOfRlyeh)
        , (36, intoTheBreach)
        , (37, rlyehRising)
        , (38, rlyehRises)
        ]
          <> [(n, breachCard n hood) | (n, hood, _) <- breaches]
      )
    & #monsters
    .~ Map.fromList [("echoes-40", cthulhu)]
    & #customEffects
    .~ Map.fromList
      [ (severKey n, severAttempt n skill EncounterTest)
      | (n, _, skill) <- breaches
      ]
    & #customAfterTests
    .~ Map.fromList
      [(severedKey n, \_ r -> when (r > 0) $ push (FlipCodexCard n)) | n <- 36 : map fst3 breaches]
 where
  fst3 (a, _, _) = a

{- | Cards 32-35: the four breaches, the neighborhood each one tears open, and the
skill it takes to sever it. The River Docks breach is card 36, not one of these.
-}
breaches :: [(ArchiveNumber, NeighborhoodId, Skill)]
breaches =
  [ (32, nb "Rivertown", Strength)
  , (33, nb "Downtown", Observation)
  , (34, nb "Northside", Influence)
  , (35, nb "Miskatonic University", Will)
  ]

nb :: Text -> NeighborhoodId
nb = NeighborhoodId . coerce . spaceIdFor

number :: ArchiveNumber -> Int
number = coerce

severKey, severedKey :: ArchiveNumber -> Text
severKey n = "echoes-sever-" <> tshow (number n)
severedKey n = "echoes-severed-" <> tshow (number n)

entryOf :: ArchiveNumber -> GameM (Maybe CodexEntry)
entryOf n = uses #codex (listToMaybe . filter ((== n) . (.number)))

doomTrigger :: Text -> Int -> [Message] -> CodexTrigger
doomTrigger key n msgs =
  CodexTrigger
    { key = key
    , once = True
    , condition = \e -> do
        doom <- use #sheetDoom
        pure (not e.flipped && doom >= n)
    , action = \_ -> pushAll msgs
    }

{- | Takes a set-aside epic monster out of the pile and returns what puts it on the
board, so the caller can order it against the rest of its card.
-}
spawnAside :: CardCode -> Text -> GameM [Message]
spawnAside wanted place = do
  aside <- use (#decks . #setAside)
  found <- filterM (fmap (== wanted) . cardCode) aside
  case take 1 found of
    [cid] -> do
      #decks . #setAside %= filter (/= cid)
      pure [PlaceMonster cid (spaceIdFor place) Ready]
    _ -> do
      logText ("That epic monster is already abroad in Arkham: " <> coerce wanted)
      pure []

{- | Card 29. Three clues on the sheet and the old captain's journal turns up, which
is what lets card 31 find where Arkham and R'lyeh are joined.
-}
theStarsAreRight :: CodexBehavior
theStarsAreRight =
  defaultCodexBehavior
    { triggers =
        [ CodexTrigger
            { key = "stars-are-right"
            , once = True
            , condition = \e -> do
                clues <- use #sheetClues
                pure (not e.flipped && clues >= 3)
            , action = \_ -> push (FlipCodexCard 29)
            }
        ]
    , -- cards 32-35 stay in the archive; card 31 is what deals them out
      onFlip = \e -> when e.flipped $ pushAll [AddArchiveToCodex 31, RemoveCodexCard 29]
    }

-- | Card 30. Four doom on the sheet and the Servitor comes ashore at the Unvisited Isle.
echoesOfTheDeep :: CodexBehavior
echoesOfTheDeep =
  defaultCodexBehavior
    { triggers = [doomTrigger "echoes-of-the-deep" 4 [FlipCodexCard 30]]
    , onFlip = \e -> when e.flipped do
        spawn <- spawnAside "echoes-39" "Unvisited Isle"
        pushAll (spawn <> [AddArchiveToCodex 37, RemoveCodexCard 30])
    }

{- | Card 31. Two clues off the sheet at the Observatory locate one breach at random;
once all four are severed the investigators make for the River Docks, unless
Cthulhu beat them to it.
-}
lostCityOfRlyeh :: CodexBehavior
lostCityOfRlyeh =
  defaultCodexBehavior
    { componentActions =
        [ ComponentActionDef
            { label = "Find where Arkham and R'lyeh are connected"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                me <- entryOf 31
                at' <- (== Just (spaceIdFor "Observatory")) <$> investigatorSpace iid
                clues <- use #sheetClues
                left <- remainingBreaches
                pure (maybe False (not . (.flipped)) me && at' && clues >= 2 && not (null left))
            , perform = \_ ->
                remainingBreaches >>= shuffle >>= \case
                  (n : _) -> pushAll [SpendSheetClues 2, AddArchiveToCodex n]
                  [] -> pure ()
            }
        ]
    , triggers =
        [ CodexTrigger
            { key = "four-markers"
            , once = True
            , condition = \e -> do
                markers <- use #sheetMarkers
                pure (not e.flipped && markers >= 4)
            , action = \_ -> push (FlipCodexCard 31)
            }
        ]
    , onFlip = \e -> when e.flipped do
        tooLate <- codexHas 38
        if tooLate
          then pushAll [LogText "Better Late than Never", RemoveCodexCard 31]
          else pushAll [LogText "To the River Docks!", AddArchiveToCodex 36, RemoveCodexCard 31]
    }

-- | The breaches card 31 has not dealt out yet, still waiting in the archive.
remainingBreaches :: GameM [ArchiveNumber]
remainingBreaches = do
  archive <- use (#decks . #archive)
  let wanted = [n | (n, _, _) <- breaches]
  defs <- traverse getCardDef archive
  pure [a.number | d <- defs, ArchiveCard a <- [d.kind], a.number `elem` wanted]

{- | Cards 32-35. Each is attached to a neighborhood deck and offers its test after
an encounter there; severing it puts a marker on the sheet, which is what card 31
counts and card 38 takes out of Cthulhu.
-}
breachCard :: ArchiveNumber -> NeighborhoodId -> CodexBehavior
breachCard n hood =
  defaultCodexBehavior
    { reactions = \e iid -> \case
        AfterEncounter who
          | who == iid
          , not e.flipped -> do
              mine <- investigatorNeighborhood iid
              pure
                [ Reaction
                    ("echoes-breach-" <> tshow (number n))
                    "Attempt to sever Arkham's connection with R'lyeh"
                    [ResolveEffect (EffectCtx iid (SourceCodex n) Nothing) (Custom (severKey n))]
                | mine == Just hood
                ]
        _ -> pure []
    , onFlip = \e -> when e.flipped $ pushAll [MarkSheet 1, RemoveCodexCard n]
    }

-- | Card 36. The last breach, closed by an action at the River Docks, and the win.
intoTheBreach :: CodexBehavior
intoTheBreach =
  defaultCodexBehavior
    { componentActions =
        [ ComponentActionDef
            { label = "Close the gate to R'lyeh"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                me <- entryOf 36
                at' <- (== Just (spaceIdFor "River Docks")) <$> investigatorSpace iid
                pure (maybe False (not . (.flipped)) me && at')
            , perform = severAttempt 36 Lore (ActionTest (ComponentAction (CodexRef 36) 0) Nothing)
            }
        ]
    , onFlip = \e ->
        when e.flipped $ pushAll [LogText "Into the Breach: Investigators win the game!", WinTheGame]
    }

{- | The sever test: a skill at -2, with a die for each remnant the investigator
cares to spend. Passing it flips whichever card asked.
-}
severAttempt :: ArchiveNumber -> Skill -> TestKind -> EffectCtx -> GameM ()
severAttempt n skill kind ctx = do
  i <- getInvestigator ctx.investigator
  if i.remnants <= 0
    then push (BeginTest (attempt 0))
    else
      chooseFor
        ctx.investigator
        "Spend remnants to roll that many additional dice"
        [ Choice
            ( TextLabel
                (if k == 0 then "Spend no remnants" else tshow k <> " remnant" <> (if k > 1 then "s" else ""))
            )
            ([PayCost ctx (SpendRemnants k) | k > 0] <> [BeginTest (attempt k)])
        | k <- [0 .. i.remnants]
        ]
 where
  attempt k =
    (newTest ctx.investigator skill (-2) kind (AfterCustom (SourceCodex n) (severedKey n)))
      { bonusDice = k
      }

{- | Card 37. Nine doom on the sheet and R'lyeh itself rises; whatever progress card
36 represented is washed away with the rest of the city.
-}
rlyehRising :: CodexBehavior
rlyehRising =
  defaultCodexBehavior
    { triggers = [doomTrigger "rlyeh-rising" 9 [FlipCodexCard 37]]
    , onFlip = \e -> when e.flipped do
        breachOpen <- codexHas 36
        spawn <- spawnAside "echoes-40" "River Docks"
        pushAll
          $ [RemoveCodexCard 36 | breachOpen]
          <> spawn
          <> [AddArchiveToCodex 38, RemoveCodexCard 37]
    }

{- | Card 38. Every marker on the sheet is three health Cthulhu does not have, and
thirteen doom on the sheet is the end of everything.
-}
rlyehRises :: CodexBehavior
rlyehRises =
  defaultCodexBehavior
    { afterMonsterDefeated = \_ mid _ -> do
        code <- cardCode mid
        pure
          [ m
          | code == "echoes-40"
          , m <-
              [ FlipCodexCard 38
              , LogText "That is Not Dead Which Can Eternal Lie: Investigators win the game!"
              , WinTheGame
              ]
          ]
    , triggers =
        [ doomTrigger
            "strange-aeons"
            13
            [ FlipCodexCard 38
            , LogText "Strange Aeons Come: Investigators lose the game."
            , LoseTheGame "Strange Aeons Come"
            ]
        ]
    }

-- | Card 40. Three health lighter for every breach that was severed before it rose.
cthulhu :: MonsterBehavior
cthulhu =
  defaultMonsterBehavior
    & #healthDelta
    .~ \_ -> do
      weakened <- codexHas 38
      markers <- use #sheetMarkers
      pure (if weakened then negate (3 * markers) else 0)
