{- | What Bound to Serve asks of the engine directly: its scenario sheet, and the
codex chain from the nightmare plague to Nyogtha's binding (cards 121-134).

The scenario has three ways out. Carl Sanford's answer to the evidence (one of
cards 131-134, drawn at random at the start) decides which: his help puts the
seals of the ancient contract on the board to be broken (card 123's front), his
refusal leaves the investigators to pay Nyogtha themselves (card 123's back), and
if the Thing Which Should Not Be gets loose first, the only end left is to bind it
again under the Witch House at the cost of everyone standing there (card 126).
-}
module AH3e.Content.SecretsOfTheOrder.BoundToServeBehaviors (behaviors) where

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
import Data.List (find, nub)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

behaviors :: Behaviors
behaviors =
  mempty
    { codex =
        Map.fromList
          [ (121, nightmarePlague)
          , (122, gatheringEvidence)
          , (123, destroyTheSeals)
          , (124, mountingDread)
          , (125, nyogthaAwakes)
          , (126, desperateBinding)
          , (127, standTogether)
          , (128, openHostility)
          , (129, closedDoors)
          , (130, byTheOrder)
          , (131, appeal 131)
          , (132, appeal 132)
          , (133, appeal 133)
          , (134, appeal 134)
          ]
    , customEffects =
        Map.fromList
          [ ("bound-to-serve-reckoning", reckoning)
          , ("bound-to-serve-spell-market", openMarket)
          , ("bound-to-serve-spell-market-close", closeMarket)
          , ("bts-plague-clue", plagueClue)
          , ("bts-evidence-reckoning", evidenceReckoning)
          , ("bts-seal-reveal", sealReveal)
          , ("bts-seal-toll", sealToll)
          , ("bts-spawn-spirit", spawnSpirit)
          , ("bts-covenant-tithe", covenantTithe)
          , ("bts-dread-discard", dreadDiscard)
          , ("bts-menace-aftermath", menaceAftermath)
          , ("bts-nyogtha-rally", nyogthaRally)
          , ("bts-binding-clue", bindingClue)
          , ("bts-stand-alone-marker", standAloneMarker)
          , ("bts-hostility-reckoning", hostilityReckoning)
          , ("bts-closed-doors-reckoning", closedDoorsReckoning)
          , ("bts-closed-doors-spawn", closedDoorsSpawn)
          , ("bts-order-reckoning", orderReckoning)
          , (oneSpell, appealSpells 1)
          , (twoSpells, appealSpells 2)
          , ("bts-binding", bindingResult)
          ]
    }

-- the scenario sheet

{- | "For each spirit monster, place one doom in its space. If there are no spirit
monsters on the board, spawn one spirit monster." The spawn is found the way a trait
is (491.3b) and put back on the bottom, so the ordinary spawn draws it and everything
that answers a monster arriving still runs.
-}
reckoning :: EffectCtx -> GameM ()
reckoning _ = do
  spirits <- uses #monsters Map.elems >>= filterM (fmap isSpirit . monsterDef . (.card))
  if null spirits
    then
      revealMonstersFromBottom "Spirit" 1 >>= \case
        (mid : _) -> do
          #decks . #monster %= (<> [mid])
          push (SpawnMonsterAt Nothing False)
        [] -> logText "No spirit monster is left to answer the pact"
    else pushAll [PlaceDoom SourceScenario m.space | m <- spirits]
 where
  isSpirit d = "Spirit" `elem` d.traits

{- | "Reveal the top three spells from the deck. You may buy any number of them. Place
the rest on the bottom of the deck. If you buy anything, gain one clue from your
neighborhood." The clue is what buying from the display calls its @ifBought@ effect,
which 'BuyFromDeck' has no room for, so the spells in hand are counted on the way in
and again once the shelves close.
-}
openMarket :: EffectCtx -> GameM ()
openMarket ctx = do
  held <- length <$> matchingAssets ctx.investigator SpellCard
  #sheetTokens . at marketKey ?= held
  pushAll
    [ ResolveEffect ctx (BuyFromDeck SpellDeckKind 3 Nothing FullPrice)
    , ResolveEffect ctx (Custom "bound-to-serve-spell-market-close")
    ]

closeMarket :: EffectCtx -> GameM ()
closeMarket ctx = do
  before <- uses #sheetTokens (Map.findWithDefault 0 marketKey)
  #sheetTokens . at marketKey .= Nothing
  held <- length <$> matchingAssets ctx.investigator SpellCard
  when (held > before) $ push (ResolveEffect ctx (GainE ClueFromNeighborhood))

marketKey :: Text
marketKey = "bound-to-serve-spells-held"

-- the places the scenario names

witchHouse, silverTwilightLodge :: SpaceId
witchHouse = spaceIdFor "The Witch House"
silverTwilightLodge = spaceIdFor "Silver Twilight Lodge"

frenchHill :: NeighborhoodId
frenchHill = NeighborhoodId (coerce (spaceIdFor "French Hill"))

-- | Where card 130 hides the seals: the oldest place in each neighborhood.
sealSpaces :: [SpaceId]
sealSpaces =
  map
    spaceIdFor
    [ "Independence Square"
    , "Unvisited Isle"
    , "Graveyard"
    , "Bayfriar Gardens"
    , "Hangman's Hill"
    , "Historical Society"
    ]

-- shared state

entryOf :: ArchiveNumber -> GameM (Maybe CodexEntry)
entryOf n = uses #codex (find ((== n) . (.number)))

inCodex :: ArchiveNumber -> GameM Bool
inCodex n = isJust <$> entryOf n

tokensOn :: ArchiveNumber -> Text -> GameM Int
tokensOn n key = maybe 0 (Map.findWithDefault 0 key . (.tokens)) <$> entryOf n

countedOn :: Text -> CodexEntry -> Int
countedOn key e = Map.findWithDefault 0 key e.tokens

-- | Whoever is still playing, for a card that acts on nobody's behalf in particular.
anyInvestigator :: GameM (Maybe InvestigatorId)
anyInvestigator = fmap (.id) . listToMaybe <$> playingInvestigators

scenarioCtx :: ArchiveNumber -> InvestigatorId -> EffectCtx
scenarioCtx n iid = EffectCtx {investigator = iid, source = SourceCodex n, testResult = Nothing}

{- | The markers moved onto the scenario sheet, by colour. The sheet itself only
counts markers, and both ways out of the contract ask which colours are there.
-}
sealColours :: [Text]
sealColours = ["red", "blue", "green"]

sealKey :: Text -> Text
sealKey colour = "bts-seal:" <> colour

sealsOnSheet :: GameM [Text]
sealsOnSheet = do
  toks <- use #sheetTokens
  pure [c | c <- sealColours, Map.findWithDefault 0 (sealKey c) toks > 0]

clearSeals :: GameM ()
clearSeals = #sheetTokens %= Map.filterWithKey (\k _ -> not (sealKey "" `T.isPrefixOf` k))

{- | A marker comes off the board and onto the scenario sheet, which is how both
of card 123's sides measure progress.
-}
moveSealToSheet :: SpaceId -> Text -> GameM ()
moveSealToSheet sid colour = do
  spaceL sid . #markers %= dropFirstMarker ((== colour) . (.color))
  #sheetTokens . at (sealKey colour) %= Just . (+ 1) . fromMaybe 0
  logText ("A " <> colour <> " marker moves to the scenario sheet")
  push (MarkSheet 1)

-- | The Lodge monsters still in the box, which several cards reach for.
lodgeAside :: GameM [CardId]
lodgeAside = use (#decks . #setAside) >>= filterM (fmap (`elem` lodgeMonsters) . cardCode)

{- | Spawns these monsters by their own spawn rules (491.3b): each goes to the
bottom of the monster deck for an ordinary spawn to draw, so everything that
answers a monster arriving still runs.
-}
spawnFromAside :: [CardId] -> GameM ()
spawnFromAside mids = unless (null mids) do
  #decks . #setAside %= filter (`notElem` mids)
  #decks . #monster %= (<> mids)
  pushAll (replicate (length mids) (SpawnMonsterAt Nothing False))

shuffleIntoMonsterDeck :: [CardId] -> GameM ()
shuffleIntoMonsterDeck mids = unless (null mids) do
  #decks . #setAside %= filter (`notElem` mids)
  deck <- use (#decks . #monster)
  #decks . #monster <~ shuffle (deck <> mids)

whiteMarkerSpaces :: GameM [SpaceId]
whiteMarkerSpaces = map fst . filter ((== "white") . (.color) . snd) <$> allMarkers

-- | 121's clue trap: one Lodge monster is drawn into the deck for each clue taken.
plagueClue :: EffectCtx -> GameM ()
plagueClue _ = do
  aside <- lodgeAside
  drawn <- pickRandom aside
  for_ drawn \mid -> do
    #decks . #setAside %= filter (/= mid)
    #decks . #monster %= (<> [mid])
    d <- getCardDef mid
    logText (d.name <> " answers the plague from the bottom of the monster deck")
  push (MarkCodexToken 121 "clues" 1)
  logText "The clue is drawn onto Nightmare Plague"

-- 121
nightmarePlague :: CodexBehavior
nightmarePlague =
  defaultCodexBehavior
    { investigatorClueReplacement = \e iid n ->
        pure
          if e.flipped || n <= 0
            then Nothing
            else Just (replicate n (ResolveEffect (scenarioCtx 121 iid) (Custom "bts-plague-clue")))
    , triggers =
        [ CodexTrigger
            { key = "plague"
            , once = False
            , condition = \e -> pure (not e.flipped && countedOn "clues" e >= 2)
            , action = \_ -> pushAll [MarkCodexToken 121 "clues" (-2), FlipCodexCard 121]
            }
        , CodexTrigger
            { key = "dread"
            , once = True
            , condition = \e -> (\doom -> not e.flipped && doom >= 3) <$> use #sheetDoom
            , action = \_ -> mountingDreadArrives False
            }
        , CodexTrigger
            { key = "dread-told"
            , once = True
            , condition = \e -> (\doom -> e.flipped && doom >= 3) <$> use #sheetDoom
            , action = \_ -> mountingDreadArrives True
            }
        ]
    , onFlip = \e -> when e.flipped do
        -- the answer is drawn and left unread, so nobody knows which hearing they will get
        chosen <- pickRandom [131, 132, 133, 134 :: Int]
        pushAll
          $ AddArchiveToCodex 122
          : [MarkCodexToken 122 "under" n | Just n <- [chosen]]
        logText "Carl Sanford's answer waits facedown under card 122"
    }

-- | The spirits grow bolder either way; the plague itself only leaves once it is read.
mountingDreadArrives :: Bool -> GameM ()
mountingDreadArrives leaves = do
  there <- inCodex 124
  pushAll ([AddArchiveToCodex 124 | not there] <> [RemoveCodexCard 121 | leaves])

-- 122
gatheringEvidence :: CodexBehavior
gatheringEvidence =
  defaultCodexBehavior
    { componentActions =
        [ ComponentActionDef
            { label = "Present your evidence to Carl Sanford"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                me <- entryOf 122
                here <- investigatorSpace iid
                pure (maybe False (not . (.flipped)) me && here == Just silverTwilightLodge)
            , perform = \_ -> push (FlipCodexCard 122)
            }
        ]
    , onFlip = \e -> when e.flipped do
        let under = ArchiveNumber (countedOn "under" e)
        pushAll
          $ [RevealArchiveCard under [AddArchiveToCodex under] | under /= 0]
          <> [RemoveCodexCard 122]
    , reckoning = \e -> if e.flipped then Nothing else Just (Custom "bts-evidence-reckoning")
    }

{- | 122's reckoning. The doom stays on the card, so once Sanford's patience has run
out the investigators are pressed every mythos phase until they go to him.
-}
evidenceReckoning :: EffectCtx -> GameM ()
evidenceReckoning _ = do
  #codex %= map \e -> if e.number == 122 then e & #tokens %~ Map.insertWith (+) "doom" 1 else e
  doom <- tokensOn 122 "doom"
  clues <- use #sheetClues
  when (doom >= 3)
    $ chooseGroup
      "Carl Sanford grows impatient"
      [ label "Resolve a gate burst" [GateBurst]
      , label
          "Discard one clue from the scenario sheet and present the evidence now"
          ([DiscardClue Nothing | clues > 0] <> [FlipCodexCard 122])
      ]

-- 123
destroyTheSeals :: CodexBehavior
destroyTheSeals =
  defaultCodexBehavior
    { componentActions =
        [ ComponentActionDef
            { label = "Destroy a seal"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                me <- entryOf 123
                hidden <- sealsInSpaceOf iid (not . (.faceUp))
                pure (maybe False (not . (.flipped)) me && not (null hidden))
            , perform = \ctx ->
                push
                  ( BeginTest
                      ( newTest
                          ctx.investigator
                          Lore
                          0
                          (ActionTest (ComponentAction (CodexRef 123) 0) Nothing)
                          ( AfterEffect
                              ctx
                              (Seq [Custom "bts-seal-reveal", Custom "bts-seal-toll"])
                              (Custom "bts-seal-toll")
                          )
                      )
                  )
            }
        , ComponentActionDef
            { label = "Offer yourself in Arkham's place"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                me <- entryOf 123
                clues <- use #sheetClues
                offers <- covenantOffers iid
                pure (maybe False (.flipped) me && clues >= 1 && not (null offers))
            , perform = \ctx -> do
                offers <- covenantOffers ctx.investigator
                chooseFor
                  ctx.investigator
                  "Spend one clue from the scenario sheet"
                  [label lbl (SpendSheetClues 1 : msgs) | (lbl, msgs) <- offers]
            }
        ]
    , triggers =
        [ CodexTrigger
            { key = "seals"
            , once = True
            , condition = \e -> (\cs -> not e.flipped && length cs >= 3) <$> sealsOnSheet
            , action = \_ -> push (FlipCodexCard 130)
            }
        , CodexTrigger
            { key = "covenant"
            , once = True
            , condition = \e -> (\cs -> e.flipped && length cs >= 3) <$> sealsOnSheet
            , action = \_ -> do
                hostile <- inCodex 128
                closed <- inCodex 129
                pushAll
                  $ [FlipCodexCard 128 | hostile]
                  <> [FlipCodexCard 129 | closed && not hostile]
            }
        ]
    }

-- | The seal markers in an investigator's space, face up or face down.
sealsInSpaceOf :: InvestigatorId -> (Marker -> Bool) -> GameM [Marker]
sealsInSpaceOf iid p = do
  msid <- investigatorSpace iid
  ms <- maybe (pure []) markersAt msid
  pure [m | m <- ms, p m, m.color `elem` sealColours]

{- | 123's front, on a pass. Which seal was hidden where is only learned by breaking
it, and a broken seal goes straight onto the scenario sheet.
-}
sealReveal :: EffectCtx -> GameM ()
sealReveal ctx = do
  msid <- investigatorSpace ctx.investigator
  hidden <- sealsInSpaceOf ctx.investigator (not . (.faceUp))
  case (msid, hidden) of
    (Just sid, m : _) -> do
      logText ("The seal is marked in " <> m.color)
      moveSealToSheet sid m.color
    _ -> pure ()

-- | 123's front, pass or fail: what the disturbed seal calls up, unless it is bought off.
sealToll :: EffectCtx -> GameM ()
sealToll ctx = do
  clues <- use #sheetClues
  if clues >= 1
    then
      chooseFor
        ctx.investigator
        "Something stirs beneath the stones"
        [ label "Spend one clue from the scenario sheet" [SpendSheetClues 1]
        , label "Spawn one spirit monster in your space" [ResolveEffect ctx (Custom "bts-spawn-spirit")]
        ]
    else push (ResolveEffect ctx (Custom "bts-spawn-spirit"))

spawnSpirit :: EffectCtx -> GameM ()
spawnSpirit ctx = do
  msid <- investigatorSpace ctx.investigator
  for_ msid \sid ->
    revealMonstersFromBottom "Spirit" 1 >>= \case
      (mid : _) -> push (PlaceMonster mid sid Ready)
      [] -> logText "No spirit monster is left to answer the pact"

{- | 123's back. Each colour asks for something different, and only the markers
standing in the investigator's own space can be moved.
-}
covenantOffers :: InvestigatorId -> GameM [(Text, [Message])]
covenantOffers iid = do
  here <- map (.color) <$> sealsInSpaceOf iid (.faceUp)
  let ctx = scenarioCtx 123 iid
      move colour =
        ResolveEffect ctx (RemoveMarkerAt YourSpace colour)
          : [MarkSheet 1, MarkSheetToken (sealKey colour) 1]
  talent <- canPayCost iid (CostDiscard (WithTrait "Talent"))
  spells <- length <$> matchingAssets iid SpellCard
  items <- matchingAssets iid ItemCard
  worth <- sum . map (fromMaybe 0) <$> traverse cardValue items
  pure
    $ [ ( "Suffer four damage and four horror to move the blue marker"
        , SufferHarm iid (SourceCodex 123) NormalHarm 4 4 : move "blue"
        )
      | "blue" `elem` here
      ]
    <> [ ( "Discard one talent to move the red marker"
         , PayCost ctx (CostDiscard (WithTrait "Talent")) : move "red"
         )
       | "red" `elem` here
       , talent
       ]
    <> [ ( "Discard two spells to move the red marker"
         , PayCost ctx (AllOf [CostDiscard SpellCard, CostDiscard SpellCard]) : move "red"
         )
       | "red" `elem` here
       , spells >= 2
       ]
    <> [ ( "Discard items worth $5 or more to move the green marker"
         , [ResolveEffect ctx (Custom "bts-covenant-tithe")]
         )
       | "green" `elem` here
       , worth >= 5
       ]

titheKey :: Text
titheKey = "bts-covenant-tithe"

{- | The green seal's price, paid one item at a time until it is met; the marker
only moves once the whole five dollars are on the table.
-}
covenantTithe :: EffectCtx -> GameM ()
covenantTithe ctx = do
  paid <- uses #sheetTokens (Map.findWithDefault 0 titheKey)
  if paid >= 5
    then do
      #sheetTokens . at titheKey .= Nothing
      msid <- investigatorSpace ctx.investigator
      for_ msid (`moveSealToSheet` "green")
    else do
      items <- matchingAssets ctx.investigator ItemCard
      priced <- for items \cid -> (cid,) . fromMaybe 0 <$> cardValue cid
      -- nothing left to give: the price goes unpaid and the marker stays where it is
      when (null priced) do
        #sheetTokens . at titheKey .= Nothing
        logText "There is nothing left worth giving"
      chooseFor
        ctx.investigator
        ("Discard items worth $" <> tshow (5 - paid) <> " more")
        [ Choice
            (CardLabel cid)
            [DiscardAsset cid, MarkSheetToken titheKey v, ResolveEffect ctx (Custom "bts-covenant-tithe")]
        | (cid, v) <- priced
        ]

-- 124
mountingDread :: CodexBehavior
mountingDread =
  defaultCodexBehavior
    { onAdd = \e -> unless e.flipped do
        board <- use #board
        let borders =
              nub
                [ sid
                | own <- neighborhoodSpaces frenchHill board
                , sid <- adjacentSpaces own board
                , maybe False (isStreetLike . (.kind)) (Map.lookup sid board.spaces)
                ]
        pushAll [PlaceMarker sid "white" | sid <- borders]
    , reactions = \e _ -> \case
        AfterStreetEncounter iid | not e.flipped -> do
          msid <- investigatorSpace iid
          white <- maybe (pure []) (fmap (filter ((== "white") . (.color))) . markersAt) msid
          pure
            [ Reaction
                { key = "bts-dread"
                , label = "Discard a white marker and test will"
                , messages = [ResolveEffect (scenarioCtx 124 iid) (Custom "bts-dread-discard")]
                }
            | not (null white)
            ]
        _ -> pure []
    , triggers =
        [ CodexTrigger
            { key = "dread-swells"
            , once = True
            , condition = \e -> (\doom -> not e.flipped && doom >= 6) <$> use #sheetDoom
            , action = \_ -> push (FlipCodexCard 124)
            }
        , CodexTrigger
            { key = "dread-ends"
            , once = True
            , condition = \e -> (\doom -> e.flipped && doom >= 9) <$> use #sheetDoom
            , action = \_ -> do
                awake <- inCodex 125
                pushAll ([AddArchiveToCodex 125 | not awake] <> [RemoveCodexCard 124])
            }
        ]
    , onFlip = \e -> when e.flipped deadMenace
    }

dreadDiscard :: EffectCtx -> GameM ()
dreadDiscard ctx = do
  msid <- investigatorSpace ctx.investigator
  for_ msid \sid -> do
    spaceL sid . #markers %= dropFirstMarker ((== "white") . (.color))
    logText "A white marker is discarded"
    push (ResolveEffect ctx (Test Will 0 NoEffect (SufferHorror (N 1))))

{- | 124's back. Every light left burning puts doom around it, so each one is
offered to whoever stands closest first.
-}
deadMenace :: GameM ()
deadMenace = do
  lit <- whiteMarkerSpaces
  offers <- for lit \sid -> do
    who <- nearestInvestigatorTo sid
    s <- getSpace sid
    pure
      [ ResolveEffect
          (scenarioCtx 124 iid)
          ( May
              ("Suffer two horror to quiet the spirits at " <> s.name)
              (Pay (CostHorror 2) (RemoveMarkerAt (TheSpace sid) "white"))
          )
      | Just iid <- [who]
      ]
  lead <- anyInvestigator
  pushAll
    $ concat offers
    <> [ResolveEffect (scenarioCtx 124 iid) (Custom "bts-menace-aftermath") | Just iid <- [lead]]

menaceAftermath :: EffectCtx -> GameM ()
menaceAftermath _ = do
  board <- use #board
  left <- whiteMarkerSpaces
  let around = concatMap (`adjacentSpaces` board) left
  pushAll
    $ [PlaceDoomInOrder SourceScenario around | not (null around)]
    <> [DiscardMarkers "white" | not (null left)]

-- | Whoever stands closest to a space, measured the way an investigator walks.
nearestInvestigatorTo :: SpaceId -> GameM (Maybe InvestigatorId)
nearestInvestigatorTo sid = do
  board <- use #board
  invs <- playingInvestigators
  let dist = distancesFrom (`adjacentSpaces` board) sid
      scored = [(i.id, d) | i <- invs, Just s <- [i.space], Just d <- [Map.lookup s dist]]
  pure case scored of
    [] -> Nothing
    _ -> let best = minimum (map snd scored) in listToMaybe [iid | (iid, d) <- scored, d == best]

-- 125
nyogthaAwakes :: CodexBehavior
nyogthaAwakes =
  defaultCodexBehavior
    { onAdd = \e -> unless e.flipped do
        plague <- inCodex 121
        evidence <- inCodex 122
        clues <- use #sheetClues
        when plague $ lodgeAside >>= shuffleIntoMonsterDeck
        lead <- anyInvestigator
        pushAll
          $ [PlaceDoomOnSheet 2 | plague]
          <> [SpendSheetClues (clues `div` 2) | evidence]
          <> [FlipCodexCard 122 | evidence]
          <> [ResolveEffect (scenarioCtx 125 iid) (Custom "bts-nyogtha-rally") | Just iid <- [lead]]
    , triggers =
        [ CodexTrigger
            { key = "nyogtha-free"
            , once = True
            , condition = \e -> do
                doom <- use #sheetDoom
                sealed <- (>= 5) <$> tokensOn 126 "clues"
                pure (not e.flipped && doom >= 13 && not sealed)
            , action = \_ -> push (FlipCodexCard 125)
            }
        ]
    , onFlip = \e -> when e.flipped $ push (LoseTheGame "Nyogtha Awakes!")
    }

{- | 125, once the Lodge's answer has been read: whether the investigators face
Nyogtha with the Order behind them decides which side of card 127 they get.
-}
nyogthaRally :: EffectCtx -> GameM ()
nyogthaRally _ = do
  order <- inCodex 130
  gone <- filterM inCodex [121, 122, 123]
  pushAll
    $ [if order then AddArchiveToCodex 127 else AddArchiveToCodexFlipped 127]
    <> map RemoveCodexCard gone
    <> [AddArchiveToCodex 126]

-- 126
desperateBinding :: CodexBehavior
desperateBinding =
  defaultCodexBehavior
    { onAdd = \e -> unless e.flipped do
        marked <- use #sheetMarkers
        #sheetMarkers .= 0
        clearSeals
        #unstableSpace ?= witchHouse
        logText "The Witch House is the unstable space"
        pushAll [MarkCodexToken 126 "clues" marked | marked > 0]
    , componentActions =
        [ ComponentActionDef
            { label = "Seal Nyogtha beneath the Witch House"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                me <- entryOf 126
                here <- investigatorSpace iid
                pure (maybe False (not . (.flipped)) me && here == Just witchHouse)
            , perform = \ctx ->
                push
                  ( BeginTest
                      ( newTest
                          ctx.investigator
                          Lore
                          (-1)
                          (ActionTest (ComponentAction (CodexRef 126) 0) Nothing)
                          (AfterEffect ctx (Custom "bts-binding") NoEffect)
                      )
                  )
            }
        ]
    , triggers =
        [ CodexTrigger
            { key = "sealed"
            , once = True
            , condition = \e -> pure (not e.flipped && countedOn "clues" e >= 5)
            , action = \_ -> push (FlipCodexCard 126)
            }
        ]
    , onFlip = \e -> when e.flipped do
        invs <- playingInvestigators
        {- Winning is the first thing the back of the card says, so the price is
        paid where they stand: a devour would end the turn and unwind a whole round
        of play before the win could land. -}
        for_ [i.id | i <- invs, i.space == Just witchHouse] \iid -> do
          investigatorL iid %= \inv -> inv {status = Devoured, space = Nothing}
          d <- getInvestigatorDef iid
          logText (d.name <> " is devoured beneath the seal")
        pushAll [LogText "The seal holds, and holds them with it", WinTheGame]
    }

{- | Each success is one more clue that may be poured into the seal, and each one
costs its pourer blood. The nesting is how "for each success, you may" is asked:
one offer at a time, stopping as soon as one is declined.
-}
bindingResult :: EffectCtx -> GameM ()
bindingResult ctx = do
  clues <- use #sheetClues
  let offers = min (fromMaybe 0 ctx.testResult) clues
  when (offers > 0) $ push (ResolveEffect ctx (nest offers))
 where
  nest 0 = NoEffect
  nest k =
    MayPay (AllOf [CostDamage 1, CostHorror 1]) (Seq [Custom "bts-binding-clue", nest (k - 1)]) NoEffect

bindingClue :: EffectCtx -> GameM ()
bindingClue _ = do
  clues <- use #sheetClues
  when (clues >= 1) $ pushAll [SpendSheetClues 1, MarkCodexToken 126 "clues" 1]

-- 127

markerKey :: InvestigatorId -> Text
markerKey iid = "white:" <> coerce iid

grantKey :: Text
grantKey = "bts-white-marker-action"

standTogether :: CodexBehavior
standTogether =
  defaultCodexBehavior
    { onAdd = \_ -> do
        drawn <- use #drawnTokens
        #drawnTokens .= []
        returnTokensToCup (drawn <> replicate 5 WhiteMarkerToken)
        logText "Five white markers go into the mythos cup"
    , tokenDrawn = \e iid -> \case
        WhiteMarkerToken
          | e.flipped -> pure [ResolveEffect (scenarioCtx 127 iid) (Custom "bts-stand-alone-marker")]
          | otherwise ->
              pure [MarkCodexToken 127 (markerKey iid) 1, LogText "A white marker joins your play area"]
        _ -> pure []
    , freeActions =
        [ ComponentActionDef
            { label = "Return a white marker for an additional action"
            , allowedWhileEngaged = True
            , canPerform = \iid -> do
                me <- entryOf 127
                spent <- usedAbility iid grantKey
                held <- tokensOn 127 (markerKey iid)
                pure (maybe False (not . (.flipped)) me && held > 0 && not spent)
            , perform = \ctx -> do
                spendOncePerRound ctx.investigator grantKey
                pushAll
                  [ MarkCodexToken 127 (markerKey ctx.investigator) (-1)
                  , GrantAnotherAction ctx.investigator
                  ]
            }
        ]
    , triggers =
        [ CodexTrigger
            { key = "cup-empty"
            , once = True
            , condition = \e -> (\cup -> e.flipped && null cup) <$> use #cup
            , action = \_ -> pushAll [DiscardMarkers "white", RemoveCodexCard 127]
            }
        ]
    }

-- | 127's back: the marker falls where its drawer stands, and every one there bites.
standAloneMarker :: EffectCtx -> GameM ()
standAloneMarker ctx = do
  msid <- investigatorSpace ctx.investigator
  for_ msid \sid -> do
    spaceL sid . #markers %= (<> [Marker "white" True])
    white <- length . filter ((== "white") . (.color)) <$> markersAt sid
    logText "A white marker falls out of the dark"
    push (ResolveEffect ctx (SufferHarmE (N white) (N white)))

-- 128
openHostility :: CodexBehavior
openHostility =
  defaultCodexBehavior
    { onAdd = \e -> unless e.flipped do
        aside <- lodgeAside
        hunters <- take 2 <$> shuffle aside
        shuffleIntoMonsterDeck (filter (`notElem` hunters) aside)
        #decks . #setAside %= filter (`notElem` hunters)
        #decks . #monster %= (<> hunters)
        #cup %= (<> [SpawnMonsterToken, SpreadDoomToken])
        seals <- sealMessages True
        pushAll (replicate (length hunters) (SpawnMonsterAt Nothing False) <> seals)
    , reckoning = \e -> if e.flipped then Nothing else Just (Custom "bts-hostility-reckoning")
    , onFlip = \e -> when e.flipped $ pushAll [LogText "Open Hostility: the contract is undone", WinTheGame]
    }

hostilityReckoning :: EffectCtx -> GameM ()
hostilityReckoning _ = do
  ms <- uses #monsters Map.elems >>= filterM (fmap (`elem` lodgeMonsters) . cardCode . (.card))
  found <- revealMonstersFromBottom "Lodge" 1
  -- the doom falls before the new arrival, so it is one list and not two pushes
  arrival <- case found of
    (mid : _) -> do
      #decks . #monster %= (<> [mid])
      pure [SpawnMonsterAt Nothing False]
    [] -> [] <$ logText "No Lodge monster is left to send"
  pushAll ([PlaceDoom SourceScenario m.space | m <- ms] <> arrival)

-- 129
closedDoors :: CodexBehavior
closedDoors =
  defaultCodexBehavior
    { onAdd = \e -> unless e.flipped do
        aside <- lodgeAside
        chosen <- take 3 <$> shuffle aside
        shuffleIntoMonsterDeck chosen
        sealMessages True >>= pushAll
    , reckoning = \e -> if e.flipped then Nothing else Just (Custom "bts-closed-doors-reckoning")
    , onFlip = \e -> when e.flipped $ pushAll [LogText "Closed Doors: the contract is undone", WinTheGame]
    }

closedDoorsReckoning :: EffectCtx -> GameM ()
closedDoorsReckoning ctx = do
  aside <- lodgeAside
  unstable <- unstableSpaces
  chooseGroup
    "The Lodge turns its back"
    $ [ label "Place one doom at the unstable space" [PlaceDoom SourceScenario sid]
      | sid <- take 1 unstable
      ]
    <> [ label
           "Spawn one random set-aside Lodge monster"
           [ResolveEffect ctx (Custom "bts-closed-doors-spawn")]
       | not (null aside)
       ]

closedDoorsSpawn :: EffectCtx -> GameM ()
closedDoorsSpawn _ = do
  aside <- lodgeAside
  chosen <- pickRandom aside
  case chosen of
    Just mid -> spawnFromAside [mid]
    Nothing -> logText "No Lodge monster is left in the box"

-- 130
byTheOrder :: CodexBehavior
byTheOrder =
  defaultCodexBehavior
    { onAdd = \e -> unless e.flipped do
        onBoard <- uses #monsters Map.elems >>= filterM (fmap (`elem` lodgeMonsters) . cardCode . (.card))
        inDeck <- use (#decks . #monster) >>= filterM (fmap (`elem` lodgeMonsters) . cardCode)
        let boxed = map (.card) onBoard <> inDeck
        for_ boxed \mid -> do
          #monsters . at mid .= Nothing
          #provoked . at mid .= Nothing
        #decks . #monster %= filter (`notElem` boxed)
        #decks . #removed %= (boxed <>)
        unless (null boxed) $ logText "The Lodge calls its own back out of Arkham"
        sealMessages False >>= pushAll
        invs <- playingInvestigators
        chooseGroup
          "Choose an investigator to become the Steward of the Order"
          [Choice (InvestigatorLabel i.id) [GainNamedCard i.id "Steward of the Order"] | i <- invs]
    , reckoning = \e -> if e.flipped then Nothing else Just (Custom "bts-order-reckoning")
    , onFlip = \e -> when e.flipped $ pushAll [LogText "By the Order: the final seal is sundered", WinTheGame]
    }

orderReckoning :: EffectCtx -> GameM ()
orderReckoning ctx = do
  ms <- uses #monsters Map.elems
  chooseGroup
    "By the Order"
    $ [Choice (MonsterLabel m.card) [DiscardMonster m.card] | m <- ms]
    <> [label "Remove one doom from any space" [ResolveEffect ctx (RemoveDoomFrom AnySpace (N 1))]]

{- | The seals of the ancient contract, as 128, 129 and 130 put them out. The Lodge
on side leaves them hidden one to a neighborhood; the Lodge against leaves all three
in the Witch House, to be bought back one at a time. Either way, Nyogtha already
loose means there are no seals left to break.
-}
sealMessages :: Bool -> GameM [Message]
sealMessages hostile = do
  awake <- inCodex 125
  if awake
    then pure []
    else
      if hostile
        then pure (map (PlaceMarker witchHouse) sealColours <> [AddArchiveToCodexFlipped 123])
        else do
          board <- use #board
          colours <- shuffle (concatMap (replicate 2) sealColours)
          let there = filter (`Map.member` board.spaces) sealSpaces
          pure (zipWith PlaceMarkerFacedown there colours <> [AddArchiveToCodex 123])

oneSpell, twoSpells :: Text
oneSpell = "bts-appeal-one-spell"
twoSpells = "bts-appeal-two-spells"

-- | Card 132's reward: the Lodge opens its library, and the group says who reads it.
appealSpells :: Int -> EffectCtx -> GameM ()
appealSpells k ctx = do
  invs <- playingInvestigators
  chooseGroup
    ("Choose an investigator to gain " <> (if k == 1 then "one spell" else tshow k <> " spells"))
    [ Choice
        (InvestigatorLabel i.id)
        [ResolveEffect (ctx & #investigator .~ i.id) (Seq (replicate k (GainE (ASpell Nothing))))]
    | i <- invs
    ]

{- | Cards 131-134. The clues on the scenario sheet are the evidence; how much of
it Sanford needs before he will lift a finger depends on which of his four answers
was drawn, and nobody knows which until it is read.
-}
appeal :: ArchiveNumber -> CodexBehavior
appeal n =
  defaultCodexBehavior
    { onAdd = \_ -> do
        clues <- use #sheetClues
        #sheetClues .= 0
        logText ("You lay out " <> tshow clues <> " clues before Carl Sanford")
        invs <- map (.id) <$> playingInvestigators
        unstable <- unstableSpaces
        let gift k =
              [ ResolveEffect (scenarioCtx n asker) (Custom (if k == (1 :: Int) then oneSpell else twoSpells))
              | asker <- take 1 invs
              ]
            unstableDoom = [PlaceDoom SourceScenario sid | sid <- take 1 unstable]
        pushAll (bands clues gift unstableDoom <> [RemoveCodexCard n])
    }
 where
  hostile = [AddArchiveToCodex 128]
  closed k = [AddArchiveToCodex 129, AddSheetClues k]
  helped k = [AddArchiveToCodex 130, AddSheetClues k]
  -- the void itself, which only this answer opens (core set card 25)
  plumbTheVoid = [AddArchiveToCodex 25]
  bands clues gift unstableDoom = case coerce n :: Int of
    131
      | clues <= 0 -> hostile <> [GateBurst]
      | clues <= 2 -> closed 1 <> [GateBurst]
      | clues <= 3 -> closed 1
      | otherwise -> helped 2
    132
      | clues <= 2 -> hostile
      | clues <= 4 -> closed 1
      | clues <= 5 -> helped 2 <> gift 1
      | otherwise -> helped 3 <> gift 2
    133
      | clues <= 0 -> hostile <> unstableDoom
      | clues <= 2 -> hostile
      | clues <= 3 -> closed 1 <> [SpreadDoom]
      | otherwise -> helped 2
    _
      | clues <= 1 -> hostile
      | clues <= 4 -> closed 1
      | clues <= 5 -> helped 2 <> plumbTheVoid
      | otherwise -> helped 3 <> plumbTheVoid
