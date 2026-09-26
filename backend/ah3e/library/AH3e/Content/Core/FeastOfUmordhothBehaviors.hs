-- | Mechanics for Feast of Umôrdhoth.
module AH3e.Content.Core.FeastOfUmordhothBehaviors (behaviors) where

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
import AH3e.Types.Ids
import AH3e.Types.State
import Data.Map.Strict qualified as Map

behaviors :: Behaviors
behaviors =
  mempty
    & #codex
    .~ Map.fromList
      [ (10, freshMeat)
      , (11, theHungerBelow)
      , (12, falseFaces)
      , (18, theFeastOfGhouls)
      ]
    & #monsters
    .~ Map.fromList
      ( ("feast-19", umordhoth)
          : [(w, defaultMonsterBehavior & #removedWhenDefeated .~ True) | w <- worshipers]
      )
    & #customEffects
    .~ Map.fromList
      [ ("feast-drew", worshiper "wolf-man-drew" Nothing)
      , ("feast-cooper", worshiper "billy-cooper" (Just "Police Station"))
      , ("feast-collins", worshiper "herman-collins" (Just "Graveyard"))
      , ("feast-turner", worshiper "ruth-turner" (Just "St. Mary's Hospital"))
      , ("feast-hill", worshiper "alma-hill" (Just "Historical Society"))
      ]

{- | Draws a set-aside worshiper onto the board, at the space its card names or
engaged with whoever turned the card up. The card that did it goes back to the
archive either way, so a worshiper already drawn out cannot be drawn again.
-}
worshiper :: CardCode -> Maybe Text -> EffectCtx -> GameM ()
worshiper wcode mplace ctx = do
  #encounter . _Just . #returnToArchive .= True
  aside <- use (#decks . #setAside)
  matching <- filterM (fmap (== wcode) . cardCode) aside
  case matching of
    [] -> logText "That worshiper is already abroad in Arkham"
    (cid : _) -> do
      #decks . #setAside %= filter (/= cid)
      name <- (.name) <$> getCardDef cid
      case mplace of
        Just place -> do
          logText (name <> " spawns at " <> place)
          push (PlaceMonster cid (spaceIdFor place) Ready)
        Nothing ->
          investigatorSpace ctx.investigator >>= traverse_ \sid -> do
            logText (name <> " spawns engaged with you")
            pushAll [PlaceMonster cid sid Ready, EngageMonster ctx.investigator cid]

-- | The monsters card 12 counts, which leave the game once they are dealt with.
worshipers :: [CardCode]
worshipers =
  ["alma-hill", "billy-cooper", "herman-collins", "masked-hunter", "ruth-turner", "wolf-man-drew"]

{- | Card 10. Three clues on the sheet buy Lita Chantler's help, which turns the
neighborhood decks into a hunt for the worshipers: one card shuffled near the top
of each, and a marker on each neighborhood to say the hunt there is still open.
-}
freshMeat :: CodexBehavior
freshMeat =
  defaultCodexBehavior
    { triggers =
        [ CodexTrigger
            { key = "fresh-meat"
            , once = True
            , condition = \e -> do
                clues <- use #sheetClues
                pure (not e.flipped && clues >= 3)
            , action = \_ -> do
                clues <- use #sheetClues
                pushAll [SpendSheetClues clues, FlipCodexCard 10]
            }
        ]
    , {- The card's own order matters: the masked hunter is abroad before the markers
      go down, and card 12 only joins the codex after that, so the hunter's own
      neighborhood keeps its marker. Since a push goes to the front of the queue,
      the whole sequence is pushed once, in the order it is printed. -}
      onFlip = \e -> when e.flipped do
        leader <- leaderPlayer >>= investigatorOfPlayer
        hunter <- maybe (pure []) (`worshiperMessages` "masked-hunter") leader
        for_ [13 .. 17 :: Int] \n -> dealHuntCard (CardCode ("feast-" <> tshow n))
        hoods <- uses (#board . #neighborhoods) Map.keys
        for_ hoods \nid -> neighborhoodL nid . #markers %= (<> [Marker "white" False])
        pushAll
          $ [GainNamedCard iid "Lita Chantler" | Just iid <- [leader]]
          <> hunter
          <> [AddArchiveToCodex 12, RemoveCodexCard 10]
    }

{- | Card 11. Eight doom on the sheet and Umordhoth arrives at Hangman's Hill,
whether or not the hunt for its worshipers ever got started.
-}
theHungerBelow :: CodexBehavior
theHungerBelow =
  defaultCodexBehavior
    { triggers =
        [ CodexTrigger
            { key = "hunger-below"
            , once = True
            , condition = \e -> do
                doom <- use #sheetDoom
                pure (not e.flipped && doom >= 8)
            , action = \_ -> push (FlipCodexCard 11)
            }
        ]
    , onFlip = \e -> when e.flipped do
        stillHunting <- uses #codex (any ((== 10) . (.number)))
        aside <- use (#decks . #setAside)
        found <- filterM (fmap (== "feast-19") . cardCode) aside
        spawn <- case take 1 found of
          [cid] -> do
            #decks . #setAside %= filter (/= cid)
            logText "Umordhoth is come"
            pure [PlaceMonster cid (spaceIdFor "Hangman's Hill") Ready]
          _ -> pure []
        pushAll
          $ [FlipCodexCard 10 | stillHunting]
          <> spawn
          <> [AddArchiveToCodex 18, RemoveCodexCard 11]
    }

{- | Card 12. Each worshiper drawn out clears the marker from its neighborhood, and
each one dealt with puts a marker on the sheet; five of them and the god itself
comes early. Its back is the win.
-}
falseFaces :: CodexBehavior
falseFaces =
  defaultCodexBehavior
    { afterMonsterSpawn = \_ mid -> do
        code <- cardCode mid
        if code `notElem` worshipers
          then pure []
          else do
            msid <- uses #monsters (fmap (.space) . Map.lookup mid)
            board <- use #board
            for_ (msid >>= (`spaceNeighborhood` board)) \nid ->
              neighborhoodL nid . #markers %= drop 1
            pure []
    , afterMonsterDefeated = \_ mid -> do
        code <- cardCode mid
        pure
          $ if code `elem` worshipers
            then [LogText "A worshiper of Umordhoth is dealt with", MarkSheet 1]
            else []
    , triggers =
        [ CodexTrigger
            { key = "false-faces-fifth-marker"
            , once = True
            , condition = \_ -> do
                markers <- use #sheetMarkers
                stillWaiting <- uses #codex (any ((== 11) . (.number)))
                pure (markers >= 5 && stillWaiting)
            , action = \_ -> push (FlipCodexCard 11)
            }
        ]
    , onFlip = \e -> when e.flipped $ push WinTheGame
    }

{- | Card 18. Every marker on the sheet has cost Umordhoth two health, and fifteen
doom on the sheet is the end of it.
-}
theFeastOfGhouls :: CodexBehavior
theFeastOfGhouls =
  defaultCodexBehavior
    { afterMonsterDefeated = \_ mid -> do
        code <- cardCode mid
        pure [FlipCodexCard 12 | code == "feast-19"]
    , triggers =
        [ CodexTrigger
            { key = "feast-of-ghouls"
            , once = True
            , condition = \e -> do
                doom <- use #sheetDoom
                pure (not e.flipped && doom >= 15)
            , action = \_ -> push (FlipCodexCard 18)
            }
        ]
    , onFlip = \e -> when e.flipped $ push (LoseTheGame "Umordhoth feasts")
    }

{- | Card 19. Two health lighter for every marker on the sheet, and its attackers may
feed it the sheet's clues for extra damage -- for an attack, damage is the result.
-}
umordhoth :: MonsterBehavior
umordhoth =
  defaultMonsterBehavior
    & #healthDelta
    .~ ( \_ -> do
           weakened <- uses #codex (any ((== 18) . (.number)))
           markers <- use #sheetMarkers
           pure (if weakened then negate (2 * markers) else 0)
       )
    & #testOptions
    .~ \_ _ ts -> do
      clues <- use #sheetClues
      pure
        [ Reaction
            ("umordhoth-clues-" <> tshow k)
            ("Umordhoth: spend " <> tshow k <> " clues from the sheet for " <> tshow k <> " more damage")
            [SpendSheetClues k, AddTestSuccesses k, ContinueTest]
        | liveDiceCount ts > 0
        , k <- [1 .. clues]
        ]

{- | Shuffles one of cards 13-17 into the top two of its own neighborhood deck, the
way the encounter decks take a card back.
-}
dealHuntCard :: CardCode -> GameM ()
dealHuntCard wanted = do
  aside <- use (#decks . #setAside)
  found <- filterM (fmap (== wanted) . cardCode) aside
  for_ (take 1 found) \cid -> do
    #decks . #setAside %= filter (/= cid)
    d <- getCardDef cid
    case d.kind of
      NeighborhoodCard nid _ -> do
        let l :: Lens' Game [CardId]
            l = #decks . #neighborhoods . at nid . non []
        deck <- use l
        l <~ shuffleIntoTopTwo cid deck
      _ -> logText "That hunt card is not a neighborhood card"

{- | Takes a set-aside worshiper out of the pile and returns what puts it on the
board engaged with one investigator, so the caller can order it against the rest.
-}
worshiperMessages :: InvestigatorId -> CardCode -> GameM [Message]
worshiperMessages iid wcode = do
  aside <- use (#decks . #setAside)
  found <- filterM (fmap (== wcode) . cardCode) aside
  case take 1 found of
    [cid] -> do
      #decks . #setAside %= filter (/= cid)
      msid <- investigatorSpace iid
      pure [m | sid <- maybeToList msid, m <- [PlaceMonster cid sid Ready, EngageMonster iid cid]]
    _ -> pure []
