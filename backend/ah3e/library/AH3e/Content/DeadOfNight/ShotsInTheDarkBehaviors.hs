{- | Mechanics for Shots in the Dark.

The two gangs each carry a standing -- unfriendly, wary, hostile or friendly --
which the codex moves around and which the scenario sheet's reckoning reads. The
sheet also collects damage and horror tokens as the investigators do one side's
work, and casualties once both sides turn on them, so all of it lives in the
sheet's named token piles.
-}
module AH3e.Content.DeadOfNight.ShotsInTheDarkBehaviors (behaviors) where

import AH3e.Content.DeadOfNight.ShotsInTheDark (setAsideMonsters)
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
      [ (41, oneMoreSpark)
      , (42, gangCard Sheldon 42)
      , (43, gangCard OBannion 43)
      , (44, allianceWork)
      , (45, playingBothSides)
      , (46, workingAnAngle)
      , (47, caught)
      , (48, hitThemHard)
      , (49, wipeThemOut)
      , (50, breakingBonds)
      , (51, seeingRed)
      , (52, shadowOfDeath)
      ]
    & #customEffects
    .~ Map.fromList
      [ ("sitd-reckoning", reckoning)
      , ("sitd-ally-lashes-out", allyLashesOut)
      , ("sitd-first-blood", firstBlood)
      , ("sitd-knifes-edge", knifesEdge)
      , ("sitd-shadow-reckoning", shadowReckoning)
      , ("sitd-refused-42", refused Sheldon)
      , ("sitd-refused-43", refused OBannion)
      , ("sitd-ally-42", \_ -> push (FlipCodexCard 42))
      , ("sitd-ally-43", \_ -> push (FlipCodexCard 43))
      , ("sitd-strike-obannion", strike OBannion)
      , ("sitd-strike-sheldon", strike Sheldon)
      , ("sitd-both-sides", \_ -> push (AddArchiveToCodex 46))
      , ("sitd-angle-spend", angleSpend)
      , ("sitd-angle-reckoning", angleReckoning)
      , ("sitd-caught-reckoning", caughtReckoning)
      , ("sitd-corben", lieutenant Sheldon)
      , ("sitd-siobhan", lieutenant OBannion)
      , ("sitd-break-hold", breakHold)
      , ("sitd-hostile-reckoning", hostileReckoning)
      , ("sitd-angle-settle", angleSettle)
      , ("sitd-angle-balance", angleBalance)
      ]

-- gang standing

{- | A gang's standing with the investigators. The sheet's own reckoning and half
the codex read it, so it lives in the sheet's token piles rather than on whichever
card happens to be face up.
-}
data Standing = Unfriendly | Wary | Hostile | Friendly
  deriving stock (Show, Eq, Enum, Bounded)

data Gang = OBannion | Sheldon
  deriving stock (Show, Eq, Enum, Bounded)

gangTrait :: Gang -> Trait
gangTrait = \case
  OBannion -> "O'Bannion"
  Sheldon -> "Sheldon"

gangName :: Gang -> Text
gangName = \case
  OBannion -> "The O'Bannions"
  Sheldon -> "The Sheldon gang"

gangKey :: Gang -> Text
gangKey = \case
  OBannion -> "obannion-standing"
  Sheldon -> "sheldon-standing"

standingOf :: Gang -> GameM Standing
standingOf g = do
  n <- uses #sheetTokens (Map.findWithDefault 0 (gangKey g))
  pure (toEnum (min (fromEnum (maxBound :: Standing)) (max 0 n)))

setStanding :: Gang -> Standing -> GameM ()
setStanding g s = do
  #sheetTokens . at (gangKey g) ?= fromEnum s
  logText (gangName g <> " are now " <> T.toLower (tshow s))

{- | The scenario sheet. While either gang is still holding back, the cult keeps
sending its own people into the streets.
-}
reckoning :: EffectCtx -> GameM ()
reckoning _ = do
  standings <- traverse standingOf [minBound .. maxBound]
  unless (Hostile `elem` standings) do
    logText "No gang is hostile, and the cult fills the gap"
    found <- revealMonstersFromBottom "Cultist" 1
    for_ (take 1 found) \mid -> do
      d <- monsterDef mid
      spaces <- ruleSpaces (Just mid) d.spawn
      case spaces of
        -- nowhere its own card would put it, so it goes back where it came from
        [] -> #decks . #monster %= (mid :)
        _ ->
          chooseGroup
            "Choose where the cultist spawns"
            (spaceChoices spaces \sid -> [PlaceMonster mid sid Ready])

entryOf :: ArchiveNumber -> GameM (Maybe CodexEntry)
entryOf n = uses #codex (listToMaybe . filter ((== n) . (.number)))

codexCtx :: ArchiveNumber -> GameM (Maybe EffectCtx)
codexCtx n = do
  ps <- playingInvestigators
  pure (case ps of i : _ -> Just (EffectCtx i.id (SourceCodex n) Nothing); [] -> Nothing)

{- | Card 41. Three tokens of any kind on the sheet and the violence turns on the
investigators; from there its reckoning feeds every human monster's space.
-}
oneMoreSpark :: CodexBehavior
oneMoreSpark =
  defaultCodexBehavior
    { triggers =
        [ CodexTrigger
            { key = "one-more-spark"
            , once = True
            , condition = \e -> do
                clues <- use #sheetClues
                doom <- use #sheetDoom
                markers <- use #sheetMarkers
                others <-
                  uses
                    #sheetTokens
                    (sum . Map.elems . Map.filterWithKey (\k _ -> notElem k (map gangKey [minBound .. maxBound])))
                pure (not e.flipped && clues + doom + markers + others >= 3)
            , action = \_ ->
                codexCtx 41 >>= traverse_ \ctx ->
                  pushAll [ResolveEffect ctx (Custom "sitd-first-blood"), AddArchiveToCodex 51, FlipCodexCard 41]
            }
        , CodexTrigger
            { key = "shadow-of-death"
            , once = True
            , condition = \e -> do
                doom <- use #sheetDoom
                pure (e.flipped && doom >= 6)
            , action = \_ -> pushAll [AddArchiveToCodex 52, RemoveCodexCard 41]
            }
        ]
    , reckoning = \e -> if e.flipped then Just (Custom "sitd-knifes-edge") else Nothing
    }

-- | "Each investigator suffers one damage or one horror for each doom on the sheet."
firstBlood :: EffectCtx -> GameM ()
firstBlood _ = do
  doom <- use #sheetDoom
  everyone <- playingInvestigators
  pushAll
    [ ResolveEffect
        (EffectCtx i.id SourceScenario Nothing)
        (Choose [("Suffer one damage", SufferDamage (N 1)), ("Suffer one horror", SufferHorror (N 1))])
    | i <- everyone
    , _ <- [1 .. doom]
    ]

-- | Card 41's back: "Place one doom in each space with one or more human monsters."
knifesEdge :: EffectCtx -> GameM ()
knifesEdge _ = do
  spaces <- spacesWithTrait "Human"
  unless (null spaces) $ push (PlaceDoomInOrder SourceScenario spaces)

-- | The spaces holding at least one monster with that trait, each named once.
spacesWithTrait :: Trait -> GameM [SpaceId]
spacesWithTrait t = do
  ms <- uses #monsters Map.elems
  matching <- filterM (fmap (elem t . (.traits)) . monsterDef . (.card)) ms
  pure (nub (map (.space) matching))

{- | Card 51. What the investigators piece together about the Cruel Hunger, and the
way through to card 50 once they have enough of it.
-}
seeingRed :: CodexBehavior
seeingRed =
  defaultCodexBehavior
    { triggers =
        [ CodexTrigger
            { key = "a-new-power"
            , once = True
            , condition = \e -> do
                clues <- use #sheetClues
                pure (not e.flipped && clues >= 4)
            , action = \_ -> push (FlipCodexCard 51)
            }
        , CodexTrigger
            { key = "breaking-bonds"
            , once = True
            , condition = \e -> do
                clues <- use #sheetClues
                pure (e.flipped && clues >= 6)
            , action = \_ -> pushAll [AddArchiveToCodex 50, RemoveCodexCard 51]
            }
        ]
    , onFlip = \e -> when e.flipped do
        #cup %= (<> [BlankToken])
        logText "A blank token joins the mythos cup"
    }

{- | Card 52. The Cruel Hunger steps closer with every death, and twelve doom on the
sheet is the end of Arkham.
-}
shadowOfDeath :: CodexBehavior
shadowOfDeath =
  defaultCodexBehavior
    { onAdd = \_ -> do
        #cup %= (<> [GateBurstToken])
        logText "A gate burst token joins the mythos cup"
    , reckoning = \_ -> Just (Custom "sitd-shadow-reckoning")
    , triggers =
        [ CodexTrigger
            { key = "the-dark-pharaoh"
            , once = True
            , condition = \e -> do
                doom <- use #sheetDoom
                pure (not e.flipped && doom >= 12)
            , action = \_ ->
                pushAll
                  [ FlipCodexCard 52
                  , LogText "The Cruel Hunger steps fully into the world: Investigators lose the game!"
                  , LoseTheGame "Shadow of Death"
                  ]
            }
        ]
    }

{- | Card 52's reckoning: "For each Human monster, place one doom in its space and
deal one horror to each investigator and ally in its neighborhood."
-}
shadowReckoning :: EffectCtx -> GameM ()
shadowReckoning _ = do
  spaces <- spacesWithTrait "Human"
  board <- use #board
  let hoods = nub [nid | sid <- spaces, Just nid <- [spaceNeighborhood sid board]]
  harmed <- fmap concat $ for hoods \nid ->
    filter (maybe False (`elem` neighborhoodSpaces nid board) . (.space)) <$> playingInvestigators
  pushAll
    $ [PlaceDoomInOrder SourceScenario spaces | not (null spaces)]
    <> [SufferHarm i.id SourceScenario NormalHarm 0 1 | i <- nub' harmed]
 where
  nub' = foldl (\acc x -> if any ((== x.id) . (.id)) acc then acc else acc <> [x]) []

-- | "An ally in your space suffers one damage and one horror."
allyLashesOut :: EffectCtx -> GameM ()
allyLashesOut ctx = do
  i <- getInvestigator ctx.investigator
  allies <- filterM (cardMatches AllyCard) i.assets
  for_ (take 1 allies) \cid -> push (HarmAsset cid 1 1)

-- | The gang muscle still waiting to be let in, by trait.
setAsideFor :: Gang -> GameM [CardId]
setAsideFor g = do
  aside <- use (#decks . #setAside)
  known <- filterM (fmap (`elem` setAsideMonsters) . cardCode) aside
  filterM (fmap (elem (gangTrait g) . (.traits)) . monsterDef) known

{- | Cards 42 and 43, one per gang. Each stands its own gang up as a possible ally:
putting the other gang's people down is what earns the meeting, and turning the
offer down only puts more of them on the streets.
-}
gangCard :: Gang -> ArchiveNumber -> CodexBehavior
gangCard gang n =
  defaultCodexBehavior
    { onAdd = \e -> unless e.flipped $ setStanding gang Unfriendly
    , afterMonsterDefeated = \e mid _ ->
        if e.flipped
          then pure []
          else do
            theirs <- monsterOf (other gang) mid
            ctx <- codexCtx n
            pure
              [ m
              | theirs
              , Just c <- [ctx]
              , m <-
                  [ ResolveEffect
                      c
                      ( Choose
                          [ ("Propose an alliance with " <> gangName gang, Custom ("sitd-ally-" <> tshow (number n)))
                          , ("Not yet", Custom ("sitd-refused-" <> tshow (number n)))
                          ]
                      )
                  ]
              ]
    , onFlip = \e -> when e.flipped do
        -- the other gang's muscle all comes out at once, and takes against you
        loose <- setAsideFor (other gang)
        #decks . #setAside %= filter (`notElem` loose)
        deck <- use (#decks . #monster)
        #decks . #monster <~ shuffle (deck <> loose)
        #cup %= (<> [if gang == Sheldon then SpreadDoomToken else SpawnMonsterToken])
        setStanding gang Wary
        setStanding (other gang) Hostile
        everyone <- playingInvestigators
        let badge = if gang == Sheldon then "LEGBREAKER" else "CLEANER"
        unless (null everyone)
          $ chooseGroup
            ("Choose an investigator to become a " <> badge)
            [Choice (InvestigatorLabel i.id) [GainNamedCard i.id badge] | i <- everyone]
        pushAll
          $ [ if gang == Sheldon then AddArchiveToCodex 44 else AddArchiveToCodexFlipped 44
            , if gang == Sheldon then AddArchiveToCodex 45 else AddArchiveToCodexFlipped 45
            , RemoveCodexCard 42
            , RemoveCodexCard 43
            ]
    }

other :: Gang -> Gang
other = \case OBannion -> Sheldon; Sheldon -> OBannion

number :: ArchiveNumber -> Int
number = coerce

monsterOf :: Gang -> CardId -> GameM Bool
monsterOf g mid = elem (gangTrait g) . (.traits) <$> monsterDef mid

{- | The offer turned down: one of the other gang's people joins the deck, and the
lead investigator answers for it to the mythos cup.
-}
refused :: Gang -> EffectCtx -> GameM ()
refused gang ctx = do
  loose <- setAsideFor (other gang)
  pickRandom loose >>= traverse_ \cid -> do
    #decks . #setAside %= filter (/= cid)
    deck <- use (#decks . #monster)
    #decks . #monster <~ shuffle (cid : deck)
    logText ("More muscle from " <> gangName (other gang) <> " takes to the streets")
  push (ResolveEffect ctx (DrawMythosTokens 1))

-- the alliance

{- | Which gang the investigators threw in with, read off card 44: its front is the
Sheldon alliance and its back the O'Bannions'.
-}
allyOf :: CodexEntry -> Gang
allyOf e = if e.flipped then OBannion else Sheldon

strongholdOf :: Gang -> GameM (Maybe SpaceId)
strongholdOf = \case
  OBannion -> markerSpace "red"
  Sheldon -> markerSpace "blue"

-- | The pile the sheet keeps for each gang's dead, which its alliance card counts.
tollKey :: Gang -> Text
tollKey = \case
  -- the O'Bannions' dead are counted in damage, the Sheldons' in horror
  OBannion -> "damage-tokens"
  Sheldon -> "horror-tokens"

tollOf :: Gang -> GameM Int
tollOf g = uses #sheetTokens (Map.findWithDefault 0 (tollKey g))

monstersOf :: Gang -> GameM [CardId]
monstersOf g = do
  ms <- uses #monsters Map.elems
  map (.card) <$> filterM (monsterOf g . (.card)) ms

{- | Card 44. The gang you have thrown in with sets you on the other, and four of
their people down is the meeting you were after.
-}
allianceWork :: CodexBehavior
allianceWork =
  defaultCodexBehavior
    { componentActions =
        [ ComponentActionDef
            { label = "Point your contact at a few jobs"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                me <- entryOf 44
                case me of
                  Nothing -> pure False
                  Just e -> do
                    home <- strongholdOf (allyOf e)
                    msid <- investigatorSpace iid
                    pure (isJust home && home == msid)
            , perform = \ctx -> do
                me <- entryOf 44
                for_ me \e -> do
                  let target = other (allyOf e)
                      reward =
                        if allyOf e == Sheldon then GainE (Money (N 1)) else Focus Nothing False
                  push
                    ( BeginTest
                        ( newTest
                            ctx.investigator
                            Influence
                            0
                            (ActionTest (ComponentAction (CodexRef 44) 0) Nothing)
                            ( AfterEffect
                                ctx
                                (Seq [Custom ("sitd-strike-" <> gangSlug target), reward])
                                NoEffect
                            )
                        )
                    )
            }
        ]
    , afterMonsterDefeated = \e mid _ -> do
        theirs <- monsterOf (other (allyOf e)) mid
        pure [MarkSheetToken (tollKey (other (allyOf e))) 1 | theirs]
    , triggers =
        [ CodexTrigger
            { key = "enough-blood"
            , once = True
            , condition = \e -> (>= 4) <$> tollOf (other (allyOf e))
            , action = \e -> do
                #sheetTokens . at (tollKey (other (allyOf e))) ?= 0
                pushAll
                  [ if allyOf e == Sheldon then AddArchiveToCodex 49 else AddArchiveToCodex 48
                  , RemoveCodexCard 44
                  , RemoveCodexCard 45
                  ]
            }
        ]
    }

gangSlug :: Gang -> Text
gangSlug = \case OBannion -> "obannion"; Sheldon -> "sheldon"

-- | "Deal two damage to a monster of that gang in any space."
strike :: Gang -> EffectCtx -> GameM ()
strike g ctx = do
  targets <- monstersOf g
  unless (null targets)
    $ chooseFor
      ctx.investigator
      ("Deal two damage to " <> gangName g)
      [Choice (MonsterLabel m) [DealMonsterDamage m (SourceCodex 44) 2] | m <- targets]

{- | Card 45. The other gang has you marked, and every reckoning they come looking
unless you buy them off with doom at their own door.
-}
playingBothSides :: CodexBehavior
playingBothSides =
  defaultCodexBehavior
    { triggers = []
    , componentActions =
        [ ComponentActionDef
            { label = "Convince the other gang you would work for them"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                me <- entryOf 44
                case me of
                  Nothing -> pure False
                  Just e -> do
                    home <- strongholdOf (other (allyOf e))
                    msid <- investigatorSpace iid
                    pure (isJust home && home == msid)
            , perform = \ctx ->
                push
                  ( BeginTest
                      ( newTest
                          ctx.investigator
                          Influence
                          (-1)
                          (ActionTest (ComponentAction (CodexRef 45) 0) Nothing)
                          (AfterEffect ctx (Custom "sitd-both-sides") NoEffect)
                      )
                  )
            }
        ]
    , reckoning = \_ -> Just (Custom "sitd-hostile-reckoning")
    }

{- | Card 46. Both gangs think you are theirs. Keeping it that way means keeping
their dead even, and the reckoning is when they compare notes.
-}
workingAnAngle :: CodexBehavior
workingAnAngle =
  defaultCodexBehavior
    { onAdd = \e -> unless e.flipped do
        loose <- use (#decks . #setAside)
        known <- filterM (fmap (`elem` setAsideMonsters) . cardCode) loose
        #decks . #setAside %= filter (`notElem` known)
        deck <- use (#decks . #monster)
        #decks . #monster <~ shuffle (deck <> known)
        for_ [minBound .. maxBound] (`setStanding` Wary)
        pushAll [RemoveCodexCard 44, RemoveCodexCard 45]
    , afterMonsterDefeated = \_ mid _ -> do
        ob <- monsterOf OBannion mid
        sh <- monsterOf Sheldon mid
        pure
          $ [MarkSheetToken (tollKey OBannion) 1 | ob]
          <> [MarkSheetToken (tollKey Sheldon) 1 | sh]
    , componentActions =
        [ ComponentActionDef
            { label = "Talk the trouble out of your space"
            , allowedWhileEngaged = True
            , canPerform = \_ -> maybe False (not . (.flipped)) <$> entryOf 46
            , perform = \ctx ->
                push
                  ( BeginTest
                      ( newTest
                          ctx.investigator
                          Influence
                          0
                          (ActionTest (ComponentAction (CodexRef 46) 0) Nothing)
                          (AfterEffect ctx (Custom "sitd-angle-spend") NoEffect)
                      )
                  )
            }
        ]
    , reckoning = \e -> if e.flipped then Nothing else Just (Custom "sitd-angle-reckoning")
    , onFlip = \e ->
        when e.flipped $ codexCtx 46 >>= traverse_ \ctx ->
          push (ResolveEffect ctx (Custom "sitd-angle-settle"))
    }

{- | "You may discard from your space a number of doom or human monsters up to your
test result." Discarded monsters are not defeated, so nothing answers them.
-}
angleSpend :: EffectCtx -> GameM ()
angleSpend ctx = go (fromMaybe 0 ctx.testResult)
 where
  go 0 = pure ()
  go n = do
    msid <- investigatorSpace ctx.investigator
    for_ msid \sid -> do
      s <- getSpace sid
      here <- uses #monsters (filter ((== sid) . (.space)) . Map.elems)
      humans <- filterM (fmap (elem "Human" . (.traits)) . monsterDef . (.card)) here
      let options =
            [Choice (TextLabel "Discard one doom") [RemoveDoom sid 1] | s.doom > 0]
              <> [Choice (MonsterLabel m.card) [DiscardMonster m.card] | m <- humans]
      unless (null options)
        $ chooseFor
          ctx.investigator
          ("Clear your space (" <> tshow n <> " left)")
          ( Choice (DoneLabel "Stop") []
              : [ Choice
                    c.label
                    (c.messages <> [ResolveEffect ctx {testResult = Just (n - 1)} (Custom "sitd-angle-spend")])
                | c <- options
                ]
          )

-- | Card 46's reckoning simply turns the card over; the reckoning is on the back.
angleReckoning :: EffectCtx -> GameM ()
angleReckoning _ = push (FlipCodexCard 46)

{- | Card 47. Both gangs have turned on you, and the bodies they leave behind are
what finally buries the city.
-}
caught :: CodexBehavior
caught =
  defaultCodexBehavior
    { onAdd = \e -> unless e.flipped do
        for_ [minBound .. maxBound] (`setStanding` Hostile)
        found <- revealMonstersFromBottom "Servitor" 1
        for_ (take 1 found) \mid -> do
          d <- monsterDef mid
          spaces <- ruleSpaces (Just mid) d.spawn
          case spaces of
            [] -> #decks . #monster %= (mid :)
            _ ->
              chooseGroup
                "Choose where the lieutenant spawns"
                (spaceChoices spaces \sid -> [PlaceMonster mid sid Ready])
    , reckoning = \_ -> Just (Custom "sitd-caught-reckoning")
    , triggers =
        [ CodexTrigger
            { key = "casualties"
            , once = True
            , condition = \e -> do
                casualties <- use #sheetMarkers
                pure (not e.flipped && casualties >= 4)
            , action = \_ ->
                pushAll
                  [ FlipCodexCard 47
                  , LogText "The body count buries Arkham: Investigators lose the game!"
                  , LoseTheGame "Caught!"
                  ]
            }
        ]
    }

-- | "The top card of the ally deck is defeated."
caughtReckoning :: EffectCtx -> GameM ()
caughtReckoning _ = do
  deck <- use (#decks . #ally)
  case deck of
    [] -> logText "There is nobody left in the ally deck to lose"
    (cid : rest) -> do
      #decks . #ally .= rest
      name <- (.name) <$> getCardDef cid
      logText (name <> " is another casualty")
      pushAll [MarkSheet 1]

{- | Cards 48 and 49. One gang is yours now; the other's property is all that is
left to break, and their lieutenant will not go quietly.
-}
hitThemHard :: CodexBehavior
hitThemHard = endgame OBannion 48 "sitd-corben" "sitd-sheldon-base"

wipeThemOut :: CodexBehavior
wipeThemOut = endgame Sheldon 49 "sitd-siobhan" "sitd-obannion-stronghold"

{- | The shape both endgame cards share: your gang goes home, theirs is swept off
the board, and what is left standing is the thing you have to knock down.
-}
endgame :: Gang -> ArchiveNumber -> Text -> CardCode -> CodexBehavior
endgame friend n reck target =
  defaultCodexBehavior
    { onAdd = \e -> unless e.flipped do
        setStanding friend Friendly
        setStanding (other friend) Hostile
        gone <- monstersOf friend
        for_ gone \mid -> do
          #monsters . at mid .= Nothing
          #decks . #removed %= (mid :)
        #decks . #monster <~ (use (#decks . #monster) >>= shuffle)
        board <- use #board
        {- Siding with the O'Bannions leaves the Sheldons' bases spread across the
        streets; siding with the Sheldons leaves the one Clover Club to burn. -}
        here <-
          if friend == OBannion
            then pure [s.id | s <- Map.elems board.spaces, nonScenicStreet s]
            else maybeToList <$> strongholdOf (other friend)
        for_ here \sid -> do
          cid <- newCard target
          spaceL sid . #markers %= (<> [Marker "white" True])
          push (PlaceMonster cid sid Ready)
        logText "What is left of them is out in the open"
    , reckoning = \_ -> Just (Custom reck)
    , -- the monster is still on the board here, so it does not count itself
      afterMonsterDefeated = \_ mid _ -> do
        code <- cardCode mid
        if code /= target
          then pure []
          else do
            others <- uses #monsters (filter ((/= mid) . (.card)) . Map.elems)
            standing <- filterM (fmap (== target) . cardCode . (.card)) others
            pure
              [ m
              | null standing
              , m <-
                  [ FlipCodexCard n
                  , LogText (gangName friend <> " and their loyal allies win the game!")
                  , WinTheGame
                  ]
              ]
    }

nonScenicStreet :: Space -> Bool
nonScenicStreet s = case s.kind of
  StreetSpace st -> st /= Scenic
  _ -> False

{- | The lieutenant each endgame card keeps sending back: Corben Bouchard shrugs off
every wound, Siobhan Riley makes somebody pay for standing near her.
-}
lieutenant :: Gang -> EffectCtx -> GameM ()
lieutenant g _ = do
  let who = if g == Sheldon then "corben-bouchard" else "siobhan-riley"
  ms <- uses #monsters Map.elems
  found <- filterM (fmap (== who) . cardCode . (.card)) ms
  case found of
    (m : _)
      | g == Sheldon -> do
          monsterL m.card . #damage .= 0
          logText "Corben Bouchard shrugs it all off"
      | otherwise -> do
          here <- investigatorsAt m.space
          pushAll [SufferHarm i.id (SourceMonster m.card) NormalHarm 1 1 | i <- take 1 here]
    [] -> do
      aside <- use (#decks . #setAside)
      deck <- use (#decks . #monster)
      pool <- filterM (fmap (== who) . cardCode) (aside <> deck)
      for_ (take 1 pool) \cid -> do
        #decks . #setAside %= filter (/= cid)
        #decks . #monster %= filter (/= cid)
        d <- monsterDef cid
        spaces <- ruleSpaces (Just cid) d.spawn
        case spaces of
          [] -> #decks . #monster %= (cid :)
          _ ->
            chooseGroup
              "Choose where the lieutenant spawns"
              (spaceChoices spaces \sid -> [PlaceMonster cid sid Ready])

{- | Card 50. The gangs were never the enemy; breaking the cult's hold on their
leaders is, and it takes the clues the investigation has banked.
-}
breakingBonds :: CodexBehavior
breakingBonds =
  defaultCodexBehavior
    { componentActions =
        [ ComponentActionDef
            { label = "Break the cult's hold"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                msid <- investigatorSpace iid
                homes <- traverse strongholdOf [minBound .. maxBound]
                pure (isJust msid && msid `elem` homes)
            , perform = \ctx -> do
                msid <- investigatorSpace ctx.investigator
                gangs <- filterM (fmap (== msid) . strongholdOf) [minBound .. maxBound]
                for_ (take 1 gangs) \g -> do
                  standing <- standingOf g
                  friendly <- hasGangAsset ctx.investigator g
                  let extra
                        | standing == Friendly = 2
                        | standing == Wary || friendly = 1
                        | otherwise = 0
                  push
                    ( BeginTest
                        ( newTest
                            ctx.investigator
                            Will
                            (-1)
                            (ActionTest (ComponentAction (CodexRef 50) 0) Nothing)
                            (AfterEffect ctx (Custom "sitd-break-hold") NoEffect)
                        )
                          { bonusDice = extra
                          }
                    )
            }
        ]
    , triggers =
        [ CodexTrigger
            { key = "bonds-broken"
            , once = True
            , condition = \e -> do
                held <- traverse heldClues [minBound .. maxBound]
                pure (not e.flipped && all (>= 4) held)
            , action = \_ ->
                pushAll
                  [ FlipCodexCard 50
                  , LogText "The gangs put down their guns: Investigators win the game!"
                  , WinTheGame
                  ]
            }
        ]
    }

heldKey :: Gang -> Text
heldKey g = gangSlug g <> "-stronghold-clues"

heldClues :: Gang -> GameM Int
heldClues g = uses #sheetTokens (Map.findWithDefault 0 (heldKey g))

-- | Whether they carry anything of that gang's, by trait or by name.
hasGangAsset :: InvestigatorId -> Gang -> GameM Bool
hasGangAsset iid g = do
  i <- getInvestigator iid
  let named d = gangTrait g `elem` d.traits
  anyOf <- filterM (fmap (maybe False named) . assetDef) i.assets
  pure (not (null anyOf))

-- | "Move a number of clues up to your test result from the sheet to a stronghold."
breakHold :: EffectCtx -> GameM ()
breakHold ctx = do
  msid <- investigatorSpace ctx.investigator
  gangs <- filterM (fmap (== msid) . strongholdOf) [minBound .. maxBound]
  clues <- use #sheetClues
  let moved = min clues (fromMaybe 0 ctx.testResult)
  for_ (take 1 gangs) \g -> when (moved > 0) do
    #sheetClues %= max 0 . subtract moved
    #sheetTokens . at (heldKey g) %= Just . (+ moved) . fromMaybe 0
    logText (tshow moved <> " clues go to " <> gangName g)
    push CheckStateTriggers

{- | Card 45's reckoning: the gang you crossed comes calling unless you point them
at their own door instead.
-}
hostileReckoning :: EffectCtx -> GameM ()
hostileReckoning _ = do
  me <- entryOf 44
  for_ me \e -> do
    let them = other (allyOf e)
    home <- strongholdOf them
    theirs <- setAsideFor them
    deck <- use (#decks . #monster)
    fromDeck <- filterM (monsterOf them) deck
    let pool = theirs <> fromDeck
    chooseGroup
      (gangName them <> " come looking for you")
      ( [ Choice (SpaceLabel sid) [PlaceDoom (SourceCodex 45) sid]
        | sid <- maybeToList home
        ]
          <> [ Choice (DoneLabel "Let them come") [PlaceMonster cid sid Ready]
             | cid <- take 1 pool
             , sid <- maybeToList home
             ]
      )

{- | Card 46's back. Even books and both gangs let it go; uneven and everybody
pays, and the second time they catch you at it there is no third.
-}
angleSettle :: EffectCtx -> GameM ()
angleSettle ctx = do
  push
    ( BeginTest
        ( newTest
            ctx.investigator
            Influence
            0
            OtherTest
            (AfterEffect ctx (Custom "sitd-angle-balance") NoEffect)
        )
    )

-- | The books, once the test has said how much of the tally you can quietly lose.
angleBalance :: EffectCtx -> GameM ()
angleBalance ctx = do
  let room = fromMaybe 0 ctx.testResult
  ob <- tollOf OBannion
  sh <- tollOf Sheldon
  -- spend the result closing whichever gap is wider
  let gap = abs (ob - sh)
      shaved = min room gap
      ob' = if ob > sh then ob - shaved else ob
      sh' = if sh > ob then sh - shaved else sh
  #sheetTokens . at (tollKey OBannion) ?= 0
  #sheetTokens . at (tollKey Sheldon) ?= 0
  if ob' == sh'
    then do
      logText "The books balance, and both gangs let it go"
      push (FlipCodexCard 46)
    else do
      logText "The books do not balance, and everybody pays"
      everyone <- playingInvestigators
      casualties <- use #sheetMarkers
      pushAll
        $ [SufferHarm i.id (SourceCodex 46) NormalHarm 1 1 | i <- everyone]
        <> ( if casualties > 0
               then [AddArchiveToCodex 47, RemoveCodexCard 46]
               else [MarkSheet 1, FlipCodexCard 46]
           )
