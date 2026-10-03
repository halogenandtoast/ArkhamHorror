-- | Tyrants of Ruin's own mechanics: its reckoning, and its codex cards.
module AH3e.Content.UnderDarkWaves.TyrantsOfRuinBehaviors (behaviors) where

import AH3e.Content.Tiles (spaceIdFor)
import AH3e.Content.UnderDarkWaves.Codex
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

behaviors :: Behaviors
behaviors =
  mempty
    & #codex
    .~ Map.fromList
      [ (61, terror)
      , (62, caughtInTheAct)
      , (63, cursedGold)
      , (64, findTheSource)
      , (65, againstTheDeep)
      , (66, fatherDagon)
      , (67, motherHydra)
      , (71, aRelicGuarded)
      , (72, actOfDesperation)
      , (73, rampage)
      ]
    & #customEffects
    .~ Map.fromList
      [ ("tyrants-reckoning", spreadTerrorWhereDeepOnesAre)
      , ("father-dagon-lurk", deepOnesRecover)
      , ("siren-call", sirenCall)
      , ("panoply-reckoning", removeOneDoom)
      , ("dagon-defeated-check", flipWhenDefeated "archive-74" 66)
      , ("hydra-defeated-check", flipWhenDefeated "archive-75" 67)
      , ("hydras-fury", hydrasFury)
      , ("dagons-rage", dagonsRage)
      , ("act-of-desperation", evenOutTheDamage)
      , ("rampage-doom", rampageDoom)
      ]
    & #customAfterTests
    .~ Map.fromList [("rally", rally)]

devilReef :: SpaceId
devilReef = spaceIdFor "Devil Reef"

{- | "Spread terror in each neighborhood with a Deep One monster." Each
neighborhood is counted once however many of them are standing in it.
-}
spreadTerrorWhereDeepOnesAre :: EffectCtx -> GameM ()
spreadTerrorWhereDeepOnesAre _ = do
  board <- use #board
  deepOnes <- deepOneMonsters
  let hoods = nub (mapMaybe ((`spaceNeighborhood` board) . (.space)) deepOnes)
  pushAll [SpreadTerror nid | nid <- hoods]

deepOneMonsters :: GameM [Monster]
deepOneMonsters = do
  ms <- uses #monsters Map.elems
  filterM (fmap (elem "Deep One" . (.traits)) . monsterDef . (.card)) ms

-- | Father Dagon's lurk: "Each Deep One monster recovers two health."
deepOnesRecover :: EffectCtx -> GameM ()
deepOnesRecover _ = do
  deepOnes <- deepOneMonsters
  for_ deepOnes \m -> #monsters . ix m.card . #damage %= max 0 . subtract 2
  unless (null deepOnes) (logText "Each Deep One monster recovers two health")

{- | Card 61, which Ithaqua's Children keeps in its codex too. The doom rule on
its front is the engine's own ('checkDoomThresholds' reads the card out of the
codex), so what is left here is the action: anyone can rally a neighborhood back
out of its terror.
-}
terror :: CodexBehavior
terror =
  defaultCodexBehavior
    & #componentActions
    .~ [ ComponentActionDef
           { label = "Organize resistance to the terror"
           , allowedWhileEngaged = False
           , canPerform = \iid -> do
               mnid <- investigatorNeighborhood iid
               maybe (pure False) (fmap terrified . getNeighborhood) mnid
           , perform = \ctx ->
               push
                 (BeginTest (newTest ctx.investigator Influence 0 OtherTest (AfterCustom (SourceCodex 61) "rally")))
           }
       ]
 where
  terrified n = n.terror > 0 || not (null n.attachedTerror)

-- | "For each success you roll, you may discard one terror token or terror card."
rally :: Source -> Int -> GameM ()
rally _ result = do
  invs <- playingInvestigators
  for_ (take 1 invs) \i -> do
    mnid <- investigatorNeighborhood i.id
    for_ mnid \nid -> do
      n <- getNeighborhood nid
      let cards = take result n.attachedTerror
          tokens = max 0 (min n.terror (result - length cards))
      neighborhoodL nid . #attachedTerror %= filter (`notElem` cards)
      neighborhoodL nid . #terror %= max 0 . subtract tokens
      #decks . #terror %= (<> cards)
      when (result > 0) (logText "The neighborhood is talked down")

{- | Card 62. The hunt is noticed, and what the investigators have not followed up
on starts to rot.
-}
caughtInTheAct :: CodexBehavior
caughtInTheAct =
  defaultCodexBehavior
    { triggers =
        [ flipOnSheetDoom "caught-in-the-act" 4 62
        , CodexTrigger
            { key = "siren-call"
            , once = True
            , condition = \e -> do
                doom <- use #sheetDoom
                pure (e.flipped && doom >= 8)
            , action = \_ -> pushAll [AddArchiveToCodex 73, RemoveCodexCard 62]
            }
        ]
    }
    & #reckoning
    .~ \e -> if e.flipped then Just (Custom "siren-call") else Nothing

-- | "Place one doom in any space in each neighborhood without a lead."
sirenCall :: EffectCtx -> GameM ()
sirenCall ctx = do
  board <- use #board
  withLeads <- markedNeighborhoods "white"
  for_ (Map.elems board.neighborhoods) \n ->
    unless (n.id `elem` withLeads)
      $ chooseGroup
        ("Place one doom in " <> n.name)
        [Choice (SpaceLabel sid) [PlaceDoom ctx.source sid] | sid <- n.spaces]

{- | Card 63. Two clues in and the gold is traced far enough to know what to look
for: the relics themselves go face down to be turned up one at a time.
-}
cursedGold :: CodexBehavior
cursedGold =
  defaultCodexBehavior
    { triggers = [flipOnSheetClues "cursed-gold" 2 63]
    , onFlip = \e -> when e.flipped do
        -- 63 leaves the codex as 64 arrives, so the relics lie under 64
        setInvestigation 64 [68, 69, 70, 71]
        pushAll [AddArchiveToCodex 64, RemoveCodexCard 63]
    }

relics :: [ArchiveNumber]
relics = [68, 69, 70]

{- | Card 64. A researched clue can be spent instead on a lead, and a lead can be
turned face down to turn up what it was pointing at.
-}
findTheSource :: CodexBehavior
findTheSource =
  defaultCodexBehavior
    { -- "instead of placing it on the scenario sheet, they may discard that clue
      -- to place a lead faceup in the neighborhood that has the most doom"
      sheetClueReplacement = \e n ->
        if e.flipped
          then pure Nothing
          else
            nextLeadNeighborhood >>= \case
              Nothing -> pure Nothing
              Just nid -> do
                hood <- getNeighborhood nid
                chooseGroup
                  "A clue researched: follow it up instead?"
                  [ Choice
                      (TextLabel ("Discard it to place a lead in " <> hood.name))
                      [PlaceNeighborhoodMarker nid "white" True]
                  , Choice (TextLabel "Add it to the scenario sheet") [AddSheetClues n]
                  ]
                pure (Just [])
    , componentActions =
        [ ComponentActionDef
            { label = "Turn a lead face down to follow it up"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                mnid <- investigatorNeighborhood iid
                case mnid of
                  Nothing -> pure False
                  Just nid -> do
                    n <- getNeighborhood nid
                    deck <- use (#decks . #investigation)
                    pure (any (\m -> m.color == "white" && m.faceUp) n.markers && not (null deck))
            , perform = \ctx -> do
                mnid <- investigatorNeighborhood ctx.investigator
                for_ mnid \nid -> do
                  neighborhoodL nid . #markers %= turnOneDown
                  found <- revealInvestigation
                  for_ found \k ->
                    if k `elem` relics
                      then do
                        waiting <- archiveHolds 65
                        push
                          ( RevealArchiveCard k
                              $ GainNamedCard ctx.investigator (relicName k)
                              : [AddArchiveToCodex 65 | waiting]
                          )
                      else push (RevealArchiveCard k [AddArchiveToCodex k])
                  -- the same here: the card that waits on the last relic is read now
                  push CheckStateTriggers
            }
        ]
    , triggers =
        [ CodexTrigger
            { key = "three-relics"
            , once = True
            , condition = \e -> do
                deck <- use (#decks . #investigation)
                pure (not e.flipped && not (any (`elem` deck) relics))
            , action = \_ -> push (FlipCodexCard 64)
            }
        ]
    , onFlip = \e -> when e.flipped do
        #board . #neighborhoods . traversed . #markers %= filter ((/= "white") . (.color))
        #decks . #investigation .= []
        #cup %= swapOne SpawnMonsterToken BlankToken
        logText "A spawn monster token leaves the mythos cup for a blank token"
    }
    & #reckoning
    .~ \e -> if e.flipped then Just (Custom "panoply-reckoning") else Nothing

relicName :: ArchiveNumber -> Text
relicName = \case
  68 -> "Headdress of Y'ha-nthlei"
  69 -> "Waveworn Idol"
  _ -> "Awakened Mantle"

-- | "the neighborhood that has the most doom and does not have a lead"
nextLeadNeighborhood :: GameM (Maybe NeighborhoodId)
nextLeadNeighborhood = do
  withLeads <- markedNeighborhoods "white"
  ranked <- byDoom
  pure (listToMaybe [n.id | n <- ranked, n.id `notElem` withLeads])

turnOneDown :: [Marker] -> [Marker]
turnOneDown ms = case break (\m -> m.color == "white" && m.faceUp) ms of
  (before, m : after) -> before <> (m {faceUp = False} : after)
  _ -> ms

swapOne :: MythosToken -> MythosToken -> [MythosToken] -> [MythosToken]
swapOne from to' = \case
  [] -> []
  x : xs | x == from -> to' : xs
  x : xs -> x : swapOne from to' xs

archiveHolds :: ArchiveNumber -> GameM Bool
archiveHolds n = do
  cards <- use (#decks . #archive)
  codes <- traverse cardCode cards
  pure (CardCode ("archive-" <> tshow (coerce n :: Int)) `elem` codes)

-- | The Panoply's reckoning: "Remove one doom from any space."
removeOneDoom :: EffectCtx -> GameM ()
removeOneDoom _ = do
  board <- use #board
  let doomed = [s.id | s <- Map.elems board.spaces, s.doom > 0]
  unless (null doomed)
    $ chooseGroup
      "Remove one doom from any space"
      [Choice (SpaceLabel sid) [RemoveDoom sid 1] | sid <- doomed]

{- | Card 65. Three relics in hand and the investigators can walk away; short of
that, one of them is spent to call a tyrant up and have it out.
-}
againstTheDeep :: CodexBehavior
againstTheDeep =
  defaultCodexBehavior
    { componentActions =
        [ ComponentActionDef
            { label = "Face what the relics called"
            , allowedWhileEngaged = False
            , canPerform = \_ -> pure True
            , perform = \ctx -> do
                relicsHeld <- relicsInPlay
                chooseFor ctx.investigator "Against the Deep"
                  $ [ label
                        "Discard a relic to call up Father Dagon"
                        (discardRelic ctx <> [AddArchiveToCodex 66])
                    | relicsHeld > 0
                    ]
                  <> [ label
                         "Discard a relic to call up Mother Hydra"
                         (discardRelic ctx <> [AddArchiveToCodex 67])
                     | relicsHeld > 0
                     ]
                  <> [label "Leave with what you have" [FlipCodexCard 65] | relicsHeld < 3]
            }
        ]
    , onFlip = \e -> when e.flipped (push WinTheGame)
    }
 where
  discardRelic ctx = [ResolveEffect ctx (Pay (CostDiscard (WithTrait "Deep One Relic")) NoEffect)]

relicsInPlay :: GameM Int
relicsInPlay = do
  invs <- playingInvestigators
  held <- for (concatMap (.assets) invs) \cid -> do
    code <- cardCode cid
    pure (code `elem` [CardCode ("archive-" <> tshow (coerce n :: Int)) | n <- relics])
  pure (length (filter id held))

{- | Cards 66 and 67. Whichever tyrant was called first is fought alone, and its
mate comes up the moment it goes down.
-}
fatherDagon, motherHydra :: CodexBehavior
fatherDagon = tyrant "archive-74" "dagon-defeated-check" "hydras-fury"
motherHydra = tyrant "archive-75" "hydra-defeated-check" "dagons-rage"

tyrant :: CardCode -> Text -> Text -> CodexBehavior
tyrant code check fury =
  defaultCodexBehavior
    { onAdd = \_ -> spawnHeldBack code devilReef >>= pushAll
    }
    & #reckoning
    .~ \e -> Just (Custom (if e.flipped then fury else check))

-- | "Reckoning -- If it has been defeated, flip this card."
flipWhenDefeated :: CardCode -> ArchiveNumber -> EffectCtx -> GameM ()
flipWhenDefeated code n _ = do
  up <- inPlay code
  held <- uses (#decks . #setAside) null
  unless (up || not held) (push (FlipCodexCard n))

-- | 66's back: Hydra comes up, and makes the investigators pay for every round.
hydrasFury :: EffectCtx -> GameM ()
hydrasFury _ = do
  up <- inPlay "archive-75"
  if up
    then do
      harm <- harmEveryone 1 1
      tollOrClue "Mother Hydra deals one damage and one horror to each investigator" harm
    else spawnHeldBack "archive-75" devilReef >>= pushAll

-- | 67's back: Dagon comes up, and keeps the reef stocked.
dagonsRage :: EffectCtx -> GameM ()
dagonsRage _ = do
  up <- inPlay "archive-74"
  if up
    then do
      ms <- uses #monsters Map.elems
      dagon <- filterM (fmap (== "archive-74") . cardCode . (.card)) ms
      for_ (take 1 dagon) \m ->
        tollOrClue
          "Spawn one Deep One monster in Father Dagon's space"
          [SpawnMonsterAt (Just m.space) False]
    else spawnHeldBack "archive-74" devilReef >>= pushAll

-- | "... unless the investigators spend a clue from the scenario sheet."
tollOrClue :: Text -> [Message] -> GameM ()
tollOrClue what toll = do
  clues <- use #sheetClues
  if clues < 1
    then pushAll toll
    else
      chooseGroup
        what
        [ Choice (TextLabel "Spend a clue from the scenario sheet") [SpendSheetClues 1]
        , Choice (TextLabel "Let it happen") toll
        ]

harmEveryone :: Int -> Int -> GameM [Message]
harmEveryone d h = do
  invs <- playingInvestigators
  pure [SufferHarm i.id (SourceCodex 66) DirectHarm d h | i <- invs]

{- | Card 71, the one card in the relic pile that is not a relic: whatever was
guarding it is still there.
-}
aRelicGuarded :: CodexBehavior
aRelicGuarded =
  defaultCodexBehavior
    { onAdd = \_ -> do
        invs <- playingInvestigators
        for_ (take 1 invs) \i -> for_ i.space \sid -> push (SpawnMonsterAt (Just sid) False)
        target <- nextLeadNeighborhood
        for_ target \nid -> push (PlaceNeighborhoodMarker nid "white" True)
        push (RemoveCodexCard 71)
    }

{- | Card 72. Once both tyrants are up, hurting one of them only moves the hurt to
the other; they have to go down together.
-}
actOfDesperation :: CodexBehavior
actOfDesperation =
  defaultCodexBehavior
    { triggers =
        [ CodexTrigger
            { key = "both-defeated"
            , once = True
            , condition = \e -> do
                dagon <- inPlay "archive-74"
                hydra <- inPlay "archive-75"
                aside <- use (#decks . #setAside)
                waiting <- filterM (fmap (`elem` ["archive-74", "archive-75"]) . cardCode) aside
                pure (not e.flipped && not dagon && not hydra && null waiting)
            , action = \_ -> push (FlipCodexCard 72)
            }
        ]
    , onFlip = \e ->
        when e.flipped do
          dagon <- inPlay "archive-74"
          hydra <- inPlay "archive-75"
          push (if dagon || hydra then LoseTheGame "They feed; they rise" else WinTheGame)
    }
    & #reckoning
    .~ \e -> if e.flipped then Nothing else Just (Custom "act-of-desperation")

{- | "Move one damage from the epic monster that has suffered the most damage to
another epic monster in any space. Repeat until all epic monsters have suffered
the same amount."
-}
evenOutTheDamage :: EffectCtx -> GameM ()
evenOutTheDamage _ = do
  ms <- uses #monsters Map.elems
  epics <- filterM (fmap (`elem` ["archive-74", "archive-75"]) . cardCode . (.card)) ms
  case sortOn (negate . (.damage)) epics of
    worst : rest@(_ : _) -> do
      let least = foldr (min . (.damage)) worst.damage rest
          shift = (worst.damage - least) `div` 2
      when (shift > 0) do
        #monsters . ix worst.card . #damage %= max 0 . subtract shift
        for_ (take 1 (sortOn (.damage) rest)) \m -> #monsters . ix m.card . #damage += shift
        logText "The tyrants share their wounds"
    _ -> pure ()

{- | Card 73. The investigation has taken too long: both tyrants come up at once,
everything the investigators had built is swept away, and the clock starts.
-}
rampage :: CodexBehavior
rampage =
  defaultCodexBehavior
    { onAdd = \_ -> do
        healed <- for ["archive-74", "archive-75"] \code -> do
          up <- inPlay code
          if up
            then do
              invs <- playingInvestigators
              ms <- uses #monsters Map.elems
              found <- filterM (fmap (== code) . cardCode . (.card)) ms
              for_ found \m -> #monsters . ix m.card . #damage %= max 0 . subtract (length invs)
              pure []
            else spawnHeldBack code devilReef
        keep <- uses #codex (map (.number))
        pushAll
          $ concat healed
          <> [RemoveCodexCard k | k <- keep, k /= 61, k /= 73]
          <> [AddArchiveToCodex 72, FlipCodexCard 73]
    , triggers =
        [ CodexTrigger
            { key = "rampage-twelve"
            , once = True
            , condition = \e -> do
                doom <- use #sheetDoom
                pure (e.flipped && doom >= 12)
            , action = \_ -> push (FlipCodexCard 72)
            }
        ]
    }
    & #reckoning
    .~ \e -> if e.flipped then Just (Custom "rampage-doom") else Nothing

-- | "Place one doom on the scenario sheet unless the investigators spend a clue."
rampageDoom :: EffectCtx -> GameM ()
rampageDoom _ = tollOrClue "Place one doom on the scenario sheet" [PlaceDoomOnSheet 1]
