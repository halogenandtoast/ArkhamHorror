-- | Mechanics for Veil of Twilight.
module AH3e.Content.Core.VeilOfTwilightBehaviors (behaviors) where

import AH3e.Content.Core.VeilOfTwilight (lodgeMonsters)
import AH3e.Content.Tiles (spaceIdFor)
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
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

behaviors :: Behaviors
behaviors =
  mempty
    & #codex
    .~ Map.fromList
      [ (20, threshold)
      , (21, lastingScars)
      , (22, goItAlone)
      , (23, someProgress)
      , (24, twilightGathers)
      , (25, plumbTheVoid)
      , (26, sanfordRevealed)
      , (27, backToTheWall)
      ]
    & #monsters
    .~ Map.fromList [("vot-28", servitor)]
    & #customEffects
    .~ Map.fromList
      [ ("vot-reckoning", reckoning)
      , ("vot-join", joinTheLodge)
      , ("vot-alliance-settled", allianceSettled)
      , ("vot-eureka", eureka)
      , ("vot-crack", crackInReality)
      , ("vot-come-undone", comeUndone)
      , ("vot-gathering-power", gatheringPower)
      , ("vot-silver-key", silverKey)
      , ("vot-like-unto-a-god", likeUntoAGod)
      , ("vot-servitor-strikes", servitorStrikes)
      , ("vot-void-step", voidStep)
      , ("vot-place-scar-1", placeScars 1)
      , ("vot-place-scar-2", placeScars 2)
      ]

-- scars

{- | A scar in the veil is a face-up white marker; mending one turns it face down,
which is what card 21 calls a mended scar.
-}
scarColour :: Text
scarColour = "white"

isScar, isMended :: Marker -> Bool
isScar m = m.color == scarColour && m.faceUp
isMended m = m.color == scarColour && not m.faceUp

scarSpaces :: GameM [SpaceId]
scarSpaces = map fst . filter (isScar . snd) <$> allMarkers

mendedCount :: GameM Int
mendedCount = length . filter (isMended . snd) <$> allMarkers

-- | The neighborhoods that already carry a scar or a mended scar.
markedNeighborhoods :: GameM [NeighborhoodId]
markedNeighborhoods = do
  board <- use #board
  marks <- filter (\(_, m) -> m.color == scarColour) <$> allMarkers
  pure (nub [nid | (sid, _) <- marks, Just nid <- [spaceNeighborhood sid board]])

-- | Every space of every neighborhood that has neither a scar nor a mended scar.
unmarkedSpaces :: GameM [SpaceId]
unmarkedSpaces = do
  board <- use #board
  marked <- markedNeighborhoods
  pure
    [ sid | nid <- Map.keys board.neighborhoods, nid `notElem` marked, sid <- neighborhoodSpaces nid board
    ]

-- | The spaces of a neighborhood with an anomaly, which card 25 steps between.
anomalySpaces :: GameM [SpaceId]
anomalySpaces = do
  board <- use #board
  pure
    [sid | (nid, n) <- Map.toList board.neighborhoods, n.anomaly, sid <- neighborhoodSpaces nid board]

entryOf :: ArchiveNumber -> GameM (Maybe CodexEntry)
entryOf n = uses #codex (listToMaybe . filter ((== n) . (.number)))

-- | An effect context for a codex card acting on its own, read from whoever is up.
codexCtx :: ArchiveNumber -> GameM (Maybe EffectCtx)
codexCtx n = do
  ps <- playingInvestigators
  pure (case ps of i : _ -> Just (EffectCtx i.id (SourceCodex n) Nothing); [] -> Nothing)

{- | A codex trigger that flips its card and hands the rest to one of the card's own
effects, so the two sides of a "flip and read" card stay apart.
-}
flipAndRead :: Text -> ArchiveNumber -> (CodexEntry -> GameM Bool) -> Effect -> CodexTrigger
flipAndRead key n cond eff =
  CodexTrigger
    { key = key
    , once = True
    , condition = cond
    , action = \_ -> codexCtx n >>= traverse_ \ctx -> pushAll [FlipCodexCard n, ResolveEffect ctx eff]
    }

doomAtLeast :: Int -> CodexEntry -> GameM Bool
doomAtLeast n e = do
  doom <- use #sheetDoom
  pure (not e.flipped && doom >= n)

mendedAtLeast :: Int -> CodexEntry -> GameM Bool
mendedAtLeast n e = do
  mended <- mendedCount
  pure (not e.flipped && mended >= n)

-- | The scenario sheet: every open scar bleeds doom into its space.
reckoning :: EffectCtx -> GameM ()
reckoning _ = do
  scars <- scarSpaces
  unless (null scars) $ push (PlaceDoomInOrder SourceScenario scars)

{- | Place this many scars, one at a time, each in a neighborhood that has none. The
space chosen settles which neighborhood it was, so one prompt does both.
-}
placeScars :: Int -> EffectCtx -> GameM ()
placeScars k ctx
  | k <= 0 = pure ()
  | otherwise = do
      spaces <- unmarkedSpaces
      unless (null spaces)
        $ chooseGroup
          "Place a scar in the veil"
          ( spaceChoices spaces \sid ->
              PlaceMarker sid scarColour
                : [ResolveEffect ctx (Custom ("vot-place-scar-" <> tshow (k - 1))) | k > 1]
          )

{- | Card 20. Two tokens on the sheet and Carl Sanford makes his offer: the ones who
take it get the Lodge's help, and if nobody does the Lodge takes the field.
-}
threshold :: CodexBehavior
threshold =
  defaultCodexBehavior
    { triggers =
        [ CodexTrigger
            { key = "threshold"
            , once = True
            , condition = \e -> do
                clues <- use #sheetClues
                doom <- use #sheetDoom
                pure (not e.flipped && clues + doom >= 2)
            , action = \_ -> push (FlipCodexCard 20)
            }
        ]
    , {- The card's own order: card 21 flips first, then the offer goes round, and
      only once everyone has answered is it settled. A push goes to the front of the
      queue, so the whole sequence is pushed once, in the order it is printed. -}
      onFlip = \e ->
        when e.flipped $ codexCtx 20 >>= traverse_ \ctx -> do
          iids <- map (.id) <$> playingInvestigators
          pushAll
            [ FlipCodexCard 21
            , ChooseInvestigatorsFor ctx (length iids) iids (Custom "vot-join")
            , ResolveEffect ctx (Custom "vot-alliance-settled")
            ]
    }

-- | One investigator takes the DARK PACT and joins the Lodge; card 20 keeps the tally.
joinTheLodge :: EffectCtx -> GameM ()
joinTheLodge ctx = do
  #codex %= map \e -> if e.number == 20 then e & #tokens %~ Map.insertWith (+) "joined" 1 else e
  logText "An investigator joins the Silver Twilight Lodge"
  push (GainConditionMsg ctx.investigator "DARK PACT")

-- | Which way card 20 went, once everyone has had the chance to join.
allianceSettled :: EffectCtx -> GameM ()
allianceSettled _ = do
  joined <- maybe 0 (Map.findWithDefault 0 "joined" . (.tokens)) <$> entryOf 20
  if joined > 0
    then pushAll [AddArchiveToCodex 24, AddArchiveToCodex 25, RemoveCodexCard 20]
    else do
      logText "Carl Sanford declares you an enemy of the Lodge"
      aside <- use (#decks . #setAside)
      lodge <- filterM (fmap (`elem` lodgeMonsters) . cardCode) aside
      #decks . #setAside %= filter (`notElem` lodge)
      deck <- use (#decks . #monster)
      #decks . #monster <~ shuffle (deck <> lodge)
      #cup %= (<> [SpawnMonsterToken, GateBurstToken])
      pushAll [AddArchiveToCodex 22, RemoveCodexCard 20]

{- | Card 21. Every anomaly leaves a scar behind, and once the card is flipped the
investigators can spend the sheet's clues to mend one.
-}
lastingScars :: CodexBehavior
lastingScars =
  defaultCodexBehavior
    { afterAnomaly = \_ nid -> do
        marked <- markedNeighborhoods
        unless (nid `elem` marked) do
          board <- use #board
          let spaces = [s | sid <- neighborhoodSpaces nid board, Just s <- [Map.lookup sid board.spaces]]
              most = maximum (0 : map (.doom) spaces)
          chooseGroup
            "Place a scar in the space with the most doom"
            (spaceChoices [s.id | s <- spaces, s.doom == most] \sid -> [PlaceMarker sid scarColour])
        pure []
    , componentActions =
        [ ComponentActionDef
            { label = "Mend a scar"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                me <- entryOf 21
                clues <- use #sheetClues
                msid <- investigatorSpace iid
                scarHere <- maybe (pure False) (fmap (any isScar) . markersAt) msid
                pure (maybe False (.flipped) me && clues >= 2 && scarHere)
            , perform = \ctx -> do
                #sheetClues %= max 0 . subtract 2
                investigatorSpace ctx.investigator >>= traverse_ \sid -> do
                  spaceL sid . #markers %= mendOne
                  logText "A scar in the veil is mended"
                push CheckStateTriggers
            }
        ]
    }

-- | Turns the first open scar face down, leaving any others as they are.
mendOne :: [Marker] -> [Marker]
mendOne ms = case break isScar ms of
  (before, m : after) -> before <> (m {faceUp = False} : after)
  _ -> ms

{- | Card 22. Nobody joined the Lodge, so the first scar mended is proof the
investigators can do this alone -- and six doom is proof they cannot.
-}
goItAlone :: CodexBehavior
goItAlone =
  defaultCodexBehavior
    { triggers =
        [ flipAndRead "eureka" 22 (mendedAtLeast 1) (Custom "vot-eureka")
        , flipAndRead "crack" 22 (doomAtLeast 6) (Custom "vot-crack")
        ]
    }

eureka :: EffectCtx -> GameM ()
eureka ctx = do
  logText "Eureka Moment!"
  pushAll [ResolveEffect ctx (Custom "vot-place-scar-2"), AddArchiveToCodex 23, RemoveCodexCard 22]

crackInReality :: EffectCtx -> GameM ()
crackInReality ctx = do
  logText "Crack in Reality"
  pushAll
    [ ResolveEffect ctx (Custom "vot-place-scar-1")
    , ResolveEffect ctx (Custom "vot-like-unto-a-god")
    , RemoveCodexCard 22
    ]

comeUndone :: EffectCtx -> GameM ()
comeUndone ctx = do
  logText "Come Undone"
  pushAll [ResolveEffect ctx (Custom "vot-like-unto-a-god"), RemoveCodexCard 23]

-- | Card 23. Three scars mended is the end of it; eight doom on the sheet is not.
someProgress :: CodexBehavior
someProgress =
  defaultCodexBehavior
    { triggers =
        [ CodexTrigger
            { key = "way-closed"
            , once = True
            , condition = mendedAtLeast 3
            , action = \_ ->
                pushAll
                  [ FlipCodexCard 23
                  , LogText "The Way is Closed: Investigators win the game!"
                  , WinTheGame
                  ]
            }
        , flipAndRead "come-undone" 23 (doomAtLeast 8) (Custom "vot-come-undone")
        ]
    }

{- | Card 24. The Lodge's own version of card 22: the first scar mended forces the
choice of whether to stay with Carl Sanford, and six doom makes it for you.
-}
twilightGathers :: CodexBehavior
twilightGathers =
  defaultCodexBehavior
    { triggers =
        [ flipAndRead "gathering-power" 24 (mendedAtLeast 1) (Custom "vot-gathering-power")
        , flipAndRead "silver-key" 24 (doomAtLeast 6) (Custom "vot-silver-key")
        ]
    }

gatheringPower :: EffectCtx -> GameM ()
gatheringPower ctx = do
  logText "Gathering Power"
  chooseGroup
    "The Lodge is storing the power it siphons. Stay, or leave?"
    [ label "Stay loyal members of the Lodge" [AddArchiveToCodex 26, RemoveCodexCard 24]
    , label
        "Betray Carl Sanford and leave"
        [ResolveEffect ctx (Custom "vot-like-unto-a-god"), RemoveCodexCard 24]
    ]

silverKey :: EffectCtx -> GameM ()
silverKey ctx = do
  logText "The Silver Key"
  pushAll
    [ ResolveEffect ctx (Custom "vot-place-scar-1")
    , ResolveEffect ctx (Custom "vot-like-unto-a-god")
    , RemoveCodexCard 24
    ]

{- | The back of card 26, which four different cards send the players to: Sanford
takes the Lurker's power and becomes its servitor.
-}
likeUntoAGod :: EffectCtx -> GameM ()
likeUntoAGod _ = do
  logText "Like Unto a God"
  spawn <- spawnAside "vot-28" "Historical Society"
  revealed <- codexHas 26
  pushAll (spawn <> [AddArchiveToCodex 27] <> [RemoveCodexCard 26 | revealed])

{- | Takes the set-aside epic monster out of the pile and returns what puts it on
the board, so the caller can order it against the rest of its card.
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

{- | Card 25. The Lodge's knowledge of the tears in reality, which steps between
anomalies -- and, flipped, the Lodge's own ending.
-}
plumbTheVoid :: CodexBehavior
plumbTheVoid =
  defaultCodexBehavior
    { componentActions =
        [ ComponentActionDef
            { label = "Step into the void"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                me <- entryOf 25
                mnid <- investigatorNeighborhood iid
                anomaly <- maybe (pure False) (fmap (.anomaly) . getNeighborhood) mnid
                pure (maybe False (not . (.flipped)) me && anomaly)
            , perform = \ctx ->
                push
                  ( BeginTest
                      ( newTest
                          ctx.investigator
                          Lore
                          (-1)
                          (ActionTest (ComponentAction (CodexRef 25) 0) Nothing)
                          ( AfterEffect
                              ctx
                              (Custom "vot-void-step")
                              (Seq [MoveDirectlyTo TheUnstableSpace, SufferHorror (N 1)])
                          )
                      )
                  )
            }
        ]
    , onFlip = \e ->
        when e.flipped
          $ pushAll [LogText "The Silver Twilight Lodge wins the game!", WinTheGame]
    }

voidStep :: EffectCtx -> GameM ()
voidStep ctx = do
  spaces <- anomalySpaces
  unless (null spaces)
    $ chooseFor
      ctx.investigator
      "Move to any space in a neighborhood with an anomaly"
      (spaceChoices spaces \sid -> [MoveDirectly ctx.investigator sid])

{- | Card 26. Sanford drinks, and the scars spread. Mending three of them while the
Lodge still holds together is the Lodge's win, not the investigators'.
-}
sanfordRevealed :: CodexBehavior
sanfordRevealed =
  defaultCodexBehavior
    { onAdd = \e ->
        unless e.flipped $ codexCtx 26 >>= traverse_ \ctx ->
          push (ResolveEffect ctx (Custom "vot-place-scar-2"))
    , triggers =
        [ CodexTrigger
            { key = "silver-twilight-wins"
            , once = True
            , condition = \e -> do
                mended <- mendedCount
                lodge <- codexHas 25
                pure (not e.flipped && mended >= 3 && lodge)
            , action = \_ -> push (FlipCodexCard 25)
            }
        , flipAndRead "like-unto-a-god" 26 (doomAtLeast 9) (Custom "vot-like-unto-a-god")
        ]
    }

{- | Card 27. Sanford is gone and the thing wearing him answers every mention of his
name in person.
-}
backToTheWall :: CodexBehavior
backToTheWall =
  defaultCodexBehavior
    { encounterOverride = \_ _ enc ->
        pure
          (if "Carl Sanford" `T.isInfixOf` enc.text then Just (Custom "vot-servitor-strikes") else Nothing)
    , afterMonsterDefeated = \_ mid _ -> do
        code <- cardCode mid
        pure
          [ m
          | code == "vot-28"
          , m <-
              [ FlipCodexCard 27
              , LogText "Arkham Scarred: Investigators win the game!"
              , WinTheGame
              ]
          ]
    , triggers =
        [ CodexTrigger
            { key = "key-and-gate"
            , once = True
            , condition = doomAtLeast 13
            , action = \_ ->
                pushAll
                  [ FlipCodexCard 27
                  , LogText "The Key and the Gate: Investigators lose the game!"
                  , LoseTheGame "The Key and the Gate"
                  ]
            }
        ]
    }

servitorStrikes :: EffectCtx -> GameM ()
servitorStrikes ctx = do
  msid <- investigatorSpace ctx.investigator
  monsters <- uses #monsters Map.elems
  found <- filterM (fmap (== "vot-28") . cardCode . (.card)) monsters
  case (msid, take 1 found) of
    (Just sid, [m]) -> do
      logText "The Servitor of Yog-Sothoth answers to that name"
      pushAll
        [ MoveMonsterTo m.card sid
        , EngageMonster ctx.investigator m.card
        , MonsterAttacks m.card ctx.investigator
        ]
    _ -> pure ()

{- | Card 28. Three health for every scar still open, and stepping away from it tears
the veil where you stand.
-}
servitor :: MonsterBehavior
servitor =
  defaultMonsterBehavior
    & #healthDelta
    .~ (\_ -> (3 *) . length <$> scarSpaces)
    & #afterDisengage
    .~ \mid iid -> do
      msid <- investigatorSpace iid
      pure [PlaceDoom (SourceMonster mid) sid | sid <- maybeToList msid]
