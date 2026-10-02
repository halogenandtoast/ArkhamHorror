-- | What the Under Dark Waves investigators' own cards do.
module AH3e.Content.UnderDarkWaves.StartingBehaviors (behaviors) where

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
    { assets =
        Map.fromList
          [ ("anticipation", anticipation)
          , ("as-you-wish", asYouWish)
          , ("prepared-for-anything", preparedForAnything)
          , ("voice-of-authority", voiceOfAuthority)
          , ("bonnie-walsh", bonnieWalsh)
          , ("calling-in-favors", callingInFavors)
          , ("signum-crucis", signumCrucis)
          , ("hold-back-the-darkness", holdBackTheDarkness)
          , ("holy-water", holyWater)
          , ("patrices-violin", patricesViolin)
          , ("captivating-melody", captivatingMelody)
          , ("ominous-dreams", ominousDreams)
          , ("fishing-net", fishingNet)
          , ("flannel-shirt", flannelShirt)
          , ("delivery-truck", deliveryTruck)
          , ("snow-nor-rain", snowNorRain)
          , ("called-by-the-mists", calledByTheMists)
          , ("chefs-knife", chefsKnife)
          , ("zoeys-cross", zoeysCross)
          , ("enchant-weapon", enchantWeapon)
          , ("the-watcher-condition", theWatcherCondition)
          , ("wanderer", wanderer)
          ]
    , monsters = Map.fromList [("the-watcher", theWatcher)]
    , customEffects =
        Map.fromList
          [ ("shuffle-in-the-watcher", shuffleInTheWatcher)
          , ("calling-in-favors", callInFavors)
          , ("signum-crucis-boon", signumBoon)
          , ("patrices-violin", playOn)
          , ("signum-crucis-condition", shedOneCondition)
          , ("fishing-net", castTheNet)
          , ("fishing-net-recover", haulInTheNet)
          , ("enchant-weapon", bindTheSpell)
          , ("wanderer", wanderOff)
          ]
    , customAfterTests =
        Map.fromList
          [ ("calling-in-favors", favorsCollected)
          , ("signum-crucis", signumResult)
          , ("hold-back-the-darkness", darknessHeld)
          , ("zoeys-cross", crossResult)
          , ("enchant-weapon", weaponEnchanted)
          ]
    }

-- Carson Sinclair's cards --------------------------------------------------

{- | "After you perform a focus action, you may focus one additional skill for each
clue in your neighborhood."
-}
anticipation :: AssetBehavior
anticipation =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterAnyAction iid FocusAction -> do
        clues <- neighborhoodClues iid
        pure
          [ Reaction
              "anticipation"
              ("Anticipation: focus " <> tshow clues <> " more skills")
              [ ResolveEffect
                  (EffectCtx iid (SourceCard cid) Nothing)
                  (Seq (replicate clues (Focus Nothing False)))
              ]
          | clues > 0
          ]
      _ -> pure []

{- | "Once per round, after another investigator in any space performs an action, you
may spend one focus to perform that same action. (Normal action restrictions
apply.)" The restrictions are what the granted action carries: it is not allowed
to repeat one they have taken already.
-}
asYouWish :: AssetBehavior
asYouWish =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AnotherPerformsAction owner _ kind -> do
        used <- usedThisRound cid owner
        i <- getInvestigator owner
        let ctx = EffectCtx owner (SourceCard cid) Nothing
        pure
          [ Reaction
              "as-you-wish"
              "As You Wish: spend a focus to do the same"
              [ MarkAssetUsed owner cid
              , PayCost ctx (SpendFocus 1)
              , PerformGrantedAction owner kind False
              ]
          | not used
          , focusCount i > 0
          , kind `notElem` i.performed
          ]
      _ -> pure []

{- | "Once per round, when an investigator in any space suffers any amount of damage
or horror, you may discard one focus token to prevent that damage or horror."
-}
preparedForAnything :: AssetBehavior
preparedForAnything =
  defaultAssetBehavior
    & #damagePrevention
    .~ \cid iid plan -> do
      used <- usedThisRound cid iid
      i <- getInvestigator iid
      let ctx = EffectCtx iid (SourceCard cid) Nothing
      pure
        [ Reaction
            "prepared-for-anything"
            "Prepared for Anything: discard a focus to prevent all of it"
            [ MarkAssetUsed iid cid
            , PayCost ctx (SpendFocus 1)
            , PreventedHarm plan.damage plan.horror
            ]
        | not used
        , focusCount i > 0
        , plan.damage + plan.horror > 0
        ]

-- Charlie Kane's cards -----------------------------------------------------

{- | "Once per round, when resolving a test using a skill you have focused, you may
test influence in place of the indicated skill. (Original modifiers still apply.)"
Offered while the pool is still being worked out, since the skill is what the pool
is counted from.
-}
voiceOfAuthority :: AssetBehavior
voiceOfAuthority =
  defaultAssetBehavior
    & #poolOptions
    .~ \cid iid ts -> do
      used <- usedThisRound cid iid
      i <- getInvestigator iid
      pure
        [ Reaction
            "voice-of-authority"
            "Voice of Authority: test influence instead"
            [MarkAssetUsed iid cid, SetTestSkill Influence]
        | not used
        , ts.skill /= Influence
        , Map.findWithDefault 0 ts.skill i.focus > 0
        ]

-- | "Once per round, before you resolve a test, you may focus one skill of your choice."
bonnieWalsh :: AssetBehavior
bonnieWalsh =
  defaultAssetBehavior
    & #poolOptions
    .~ \cid iid _ -> do
      used <- usedThisRound cid iid
      pure
        [ Reaction
            "bonnie-walsh"
            "Bonnie Walsh: focus one skill first"
            [ MarkAssetUsed iid cid
            , ResolveEffect (EffectCtx iid (SourceCard cid) Nothing) (Focus Nothing False)
            , ContinueTest
            ]
        | not used
        ]

{- | "At the start of your turn, discard all money from this card and test influence.
For each success you roll, place $1 on this card. You may spend money from this
card." Spending it is a card fact rather than a behaviour, since a price is worked
out where behaviours cannot be reached; see
'AH3e.Content.UnderDarkWaves.Investigators.spendableMoneyCards'. The card does not
say "may", but a sheet's answer to its own turn beginning is always offered.
-}
callingInFavors :: AssetBehavior
callingInFavors =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AtStartOfTurn iid ->
        pure
          [ Reaction
              "calling-in-favors"
              "Calling in Favors: cash out and call the favors in"
              [ResolveEffect (EffectCtx iid (SourceCard cid) Nothing) (Custom "calling-in-favors")]
          ]
      _ -> pure []

callInFavors :: EffectCtx -> GameM ()
callInFavors ctx = case ctx.source of
  SourceCard cid -> do
    held <- uses #assets (maybe 0 (Map.findWithDefault 0 "money" . (.tokens)) . Map.lookup cid)
    when (held > 0) $ logText ("$" <> tshow held <> " is discarded from Calling in Favors")
    assetL cid . #tokens . at "money" ?= 0
    push
      ( BeginTest
          (newTest ctx.investigator Influence 0 OtherTest (AfterCustom (SourceCard cid) "calling-in-favors"))
      )
  _ -> pure ()

favorsCollected :: Source -> Int -> GameM ()
favorsCollected src r = case src of
  SourceCard cid -> when (r > 0) do
    assetL cid . #tokens . at "money" ?= r
    logText ("$" <> tshow r <> " is placed on Calling in Favors")
  _ -> pure ()

-- Father Mateo's cards -----------------------------------------------------

{- | "Encounter: Test will -1. For each success you roll, choose an investigator in
any space to discard one condition, recover one health, or recover one sanity."
Taking it is the encounter, which is what 'encounterAbilities' means.
-}
signumCrucis :: AssetBehavior
signumCrucis =
  defaultAssetBehavior
    & #encounterAbilities
    .~ [ ComponentActionDef
           { label = "Signum Crucis"
           , allowedWhileEngaged = True
           , canPerform = \_ -> pure True
           , perform = \ctx -> case ctx.source of
               SourceCard cid ->
                 push
                   ( BeginTest
                       (newTest ctx.investigator Will (-1) OtherTest (AfterCustom (SourceCard cid) "signum-crucis"))
                   )
               _ -> pure ()
           }
       ]

signumResult :: Source -> Int -> GameM ()
signumResult src r = case src of
  SourceCard cid -> do
    owner <- uses #assets (fmap (.owner) . Map.lookup cid)
    for_ owner \iid ->
      pushAll (replicate r (ResolveEffect (EffectCtx iid src Nothing) (Custom "signum-crucis-boon")))
  _ -> pure ()

-- | One success' worth of grace, given to whoever needs it most.
signumBoon :: EffectCtx -> GameM ()
signumBoon ctx = do
  invs <- playingInvestigators
  chooseFor
    ctx.investigator
    "Choose an investigator to bless"
    [ Choice (InvestigatorLabel i.id) [ResolveEffect (EffectCtx i.id ctx.source ctx.testResult) boon]
    | i <- invs
    ]
 where
  boon =
    Choose
      [ ("Discard one condition", Custom "signum-crucis-condition")
      , ("Recover one health", RecoverHealth You (N 1))
      , ("Recover one sanity", RecoverSanity You (N 1))
      ]

-- | The condition the signum sends away, chosen from the ones they are carrying.
shedOneCondition :: EffectCtx -> GameM ()
shedOneCondition ctx = do
  i <- getInvestigator ctx.investigator
  conditions <- filterM isCondition i.assets
  named <- for conditions \cid -> (cid,) . (.name) <$> getCardDef cid
  unless (null named)
    $ chooseFor
      ctx.investigator
      "Choose a condition to discard"
      [Choice (CardsLabel name [cid]) [DiscardAsset cid] | (cid, name) <- named]
 where
  isCondition cid =
    getCardDef cid <&> \d -> case d.kind of
      ConditionCard _ -> True
      AssetCard a -> a.assetType == ConditionAsset
      _ -> False

{- | "Once per round, before a reckoning effect resolves, you may test lore -1. If you
succeed, do not resolve that reckoning effect this mythos phase." The reckoning is
asked for again once the test has run, and the key left on the card is what tells
the test which one it was holding back.
-}
holdBackTheDarkness :: AssetBehavior
holdBackTheDarkness =
  defaultAssetBehavior
    & #beforeReckoning
    .~ \cid iid src -> do
      used <- usedThisRound cid iid
      pure
        [ Reaction
            "hold-back-the-darkness"
            "Hold Back the Darkness: test lore -1 to hold it off"
            [ MarkAssetUsed iid cid
            , RememberOnCard cid (reckoningHeldKey src)
            , BeginTest
                (newTest iid Lore (-1) OtherTest (AfterCustom (SourceCard cid) "hold-back-the-darkness"))
            , ResolveReckoning src
            ]
        | not used
        ]

darknessHeld :: Source -> Int -> GameM ()
darknessHeld src r = case src of
  SourceCard cid -> do
    keys <- uses #assets (maybe [] (Map.keys . (.tokens)) . Map.lookup cid)
    assetL cid . #tokens .= mempty
    for_ (take 1 keys) \key ->
      if r > 0
        then do
          logText "The darkness is held back"
          push (CancelReckoning key)
        else logText "The darkness comes on regardless"
  _ -> pure ()

{- | "You may treat the attack and evade modifiers of non-human monsters in your space
as +1. After you damage a non-human monster during an attack action, exhaust that
monster." The first half is always to its owner's advantage, so it is read rather
than offered; the second is offered, although the card states it flatly.
-}
holyWater :: AssetBehavior
holyWater =
  defaultAssetBehavior
    & #monsterModifierFloor
    .~ ( \_ iid mid _ -> do
           here <- investigatorSpace iid
           m <- uses #monsters (Map.lookup mid)
           inhuman <- notHuman mid
           pure $ if inhuman && isJust here && fmap (.space) m == here then Just 1 else Nothing
       )
    & #reactions
    .~ \cid -> \case
      AfterDamageMonsterInAttack iid mid -> do
        inhuman <- notHuman mid
        can <- canBeExhausted mid
        name <- (.name) <$> getCardDef mid
        pure
          [ Reaction
              "holy-water"
              ("Holy Water: exhaust " <> name)
              [MarkAssetUsed iid cid, ExhaustMonster mid]
          | inhuman
          , can
          ]
      _ -> pure []

notHuman :: CardId -> GameM Bool
notHuman mid = notElem "Human" . (.traits) <$> monsterDef mid

-- Patrice Hathaway's cards -------------------------------------------------

{- | "After you perform a gather resources action, choose a skill. Each investigator in
your neighborhood may focus that skill."
-}
patricesViolin :: AssetBehavior
patricesViolin =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterGatherResources iid ->
        pure
          [ Reaction
              "patrices-violin"
              "Patrice's Violin: play for the neighborhood"
              [ResolveEffect (EffectCtx iid (SourceCard cid) Nothing) (Custom "patrices-violin")]
          ]
      _ -> pure []

playOn :: EffectCtx -> GameM ()
playOn ctx = do
  listeners <- neighbors ctx.investigator
  unless (null listeners)
    $ chooseFor
      ctx.investigator
      "Choose the skill to play"
      [ Choice
          (SkillLabel s)
          [ ResolveEffect
              (EffectCtx who ctx.source ctx.testResult)
              (May "Focus that skill" (Focus (Just s) False))
          | who <- listeners
          ]
      | s <- allSkills
      ]

-- | Everyone standing anywhere in this investigator's neighborhood, themselves included.
neighbors :: InvestigatorId -> GameM [InvestigatorId]
neighbors iid = do
  mnid <- investigatorNeighborhood iid
  board <- use #board
  case mnid of
    Nothing -> pure [iid]
    Just nid -> do
      let sids = neighborhoodSpaces nid board
      invs <- playingInvestigators
      pure [i.id | i <- invs, maybe False (`elem` sids) i.space]

{- | "You may perform a ward action while engaged with a monster. As part of a ward
action, for each success you roll, you may exhaust one monster in your space
instead of removing one doom." Both halves are read by the ward itself.
-}
captivatingMelody :: AssetBehavior
captivatingMelody =
  defaultAssetBehavior
    & #wardWhileEngaged
    .~ True
    & #wardAlternative
    .~ True

{- | "Once per round, while resolving a test, you may reroll one success to spawn or
research one clue. If this talent is discarded, return The Watcher to the game
box." The reroll is offered as a plain reroll, so the engine does not check that
the die chosen was a success.
-}
ominousDreams :: AssetBehavior
ominousDreams =
  defaultAssetBehavior
    & #testOptions
    .~ ( \cid iid ts -> do
           used <- usedThisRound cid iid
           pure
             [ Reaction
                 "ominous-dreams"
                 "Ominous Dreams: reroll one success for a clue"
                 [ MarkAssetUsed iid cid
                 , RerollUpTo (SourceCard cid) 1
                 , ResolveEffect
                     (EffectCtx iid (SourceCard cid) Nothing)
                     (Choose [("Spawn one clue", SpawnOneClue), ("Research one clue", PlaceCluesOnSheet (N 1))])
                 ]
             | not used
             , liveDiceCount ts > 0
             ]
       )
    & #onDiscard
    .~ \_ _ -> do
      -- the dreams end, and what they summoned goes back in the box
      inPlay <- uses #monsters Map.keys
      watchers <- filterM (fmap (== "the-watcher") . cardCode) inPlay
      deck <- use (#decks . #monster)
      fromDeck <- filterM (fmap (== "the-watcher") . cardCode) deck
      conditions <- uses #assets Map.keys
      held <- filterM (fmap (== "the-watcher-condition") . cardCode) conditions
      #decks . #monster %= filter (`notElem` fromDeck)
      for_ (fromDeck <> watchers) removeCardEverywhere
      unless (null (watchers <> fromDeck <> held)) $ logText "The Watcher returns to the game box"
      pure (map DiscardAsset held)

-- Silas Marsh's cards ------------------------------------------------------

{- | "During your turn, you may attach this item to a non-epic monster in your space to
exhaust that monster. Attached: This monster cannot ready. After you defeat this
monster, gain the attached item."
-}
fishingNet :: AssetBehavior
fishingNet =
  defaultAssetBehavior
    & #stopsMonsterReady
    .~ True
    & #freeActions
    .~ [ ComponentActionDef
           { label = "Fishing Net: cast it over a monster"
           , allowedWhileEngaged = True
           , canPerform = \iid -> not . null <$> netTargets iid
           , perform = \ctx -> push (ResolveEffect ctx (Custom "fishing-net"))
           }
       ]
    & #afterMonsterDefeated
    .~ \cid iid mid _ -> do
      caught <- uses #assets (maybe False ((== Just mid) . (.attachedTo)) . Map.lookup cid)
      pure
        [ResolveEffect (EffectCtx iid (SourceCard cid) Nothing) (Custom "fishing-net-recover") | caught]

-- | Non-epic monsters standing with them that the net has not already caught.
netTargets :: InvestigatorId -> GameM [Monster]
netTargets iid = do
  here <- investigatorSpace iid
  ms <- maybe (pure []) monstersAt here
  filterM (fmap (not . (.epic)) . monsterDef . (.card)) ms

castTheNet :: EffectCtx -> GameM ()
castTheNet ctx = case ctx.source of
  SourceCard cid -> do
    ms <- netTargets ctx.investigator
    named <- for ms \m -> (m.card,) . (.name) <$> getCardDef m.card
    unless (null named)
      $ chooseFor
        ctx.investigator
        "Choose a monster to net"
        [ Choice (CardsLabel name [mid]) [AttachAsset cid mid, ExhaustMonster mid]
        | (mid, name) <- named
        ]
  _ -> pure ()

haulInTheNet :: EffectCtx -> GameM ()
haulInTheNet ctx = case ctx.source of
  SourceCard cid -> do
    assetL cid . #attachedTo .= Nothing
    assetL cid . #owner .= ctx.investigator
    logText "The Fishing Net is hauled back in"
  _ -> pure ()

{- | "Once per turn, when an investigator in your space resolves a test, you may deal
one damage to this item to add two successes to their test result." Once per turn
is kept as once per round, which is the mark the engine carries.
-}
flannelShirt :: AssetBehavior
flannelShirt =
  defaultAssetBehavior
    & #testOptions
    .~ (\cid iid _ -> shirtOffer cid iid)
    & #reactions
    .~ \cid -> \case
      AnotherResolvesTest owner tested -> do
        mine <- investigatorSpace owner
        theirs <- investigatorSpace tested
        if isJust mine && mine == theirs then shirtOffer cid owner else pure []
      _ -> pure []

shirtOffer :: CardId -> InvestigatorId -> GameM [Reaction]
shirtOffer cid iid = do
  used <- usedThisRound cid iid
  pure
    [ Reaction
        "flannel-shirt"
        "Flannel Shirt: take one damage on the shirt for two successes"
        [MarkAssetUsed iid cid, HarmAsset cid 1 0, AddTestSuccesses 2]
    | not used
    ]

-- Stella Clark's cards -----------------------------------------------------

{- | "Each time you move out of a space, any unengaged investigators in that space may
move with you. Action: Move up to two spaces. You may perform a trade action before
or after this move as an additional action." The trade is offered after the truck's
own move, which is the half of "before or after" the engine can see.
-}
deliveryTruck :: AssetBehavior
deliveryTruck =
  defaultAssetBehavior
    & #carriesPassengers
    .~ True
    & #moveAction
    ?~ (2, 0)
    & #reactions
    .~ \cid -> \case
      AfterMoveAction iid -> do
        drove <- usedThisRound cid iid
        -- whether anyone is there to trade with is the granted action's own business
        pure
          [ Reaction
              "delivery-truck"
              "Delivery Truck: trade as an additional action"
              [PerformGrantedAction iid TradeAction True]
          | drove
          ]
      _ -> pure []

{- | "Once per turn, when you would fail a test, you may discard one will focus to add
one success to your test result." Offered while the dice are still in front of
them, which is the last moment the result can be seen and changed.
-}
snowNorRain :: AssetBehavior
snowNorRain =
  defaultAssetBehavior
    & #testOptions
    .~ \cid iid _ -> do
      used <- usedThisRound cid iid
      i <- getInvestigator iid
      pure
        [ Reaction
            "snow-nor-rain"
            "Snow Nor Rain: discard a will focus for one success"
            [MarkAssetUsed iid cid, DiscardFocus iid Will, AddTestSuccesses 1, ContinueTest]
        | not used
        , Map.findWithDefault 0 Will i.focus > 0
        ]

{- | "When you would draw a mythos token, you may instead suffer one direct horror and
place one doom in your space."
-}
calledByTheMists :: AssetBehavior
calledByTheMists =
  defaultAssetBehavior
    & #replacesMythosDraw
    .~ \cid iid ->
      pure
        [ Reaction
            "called-by-the-mists"
            "Called by the Mists: take one direct horror and one doom instead"
            [ SufferHarm iid (SourceCard cid) DirectHarm 0 1
            , ResolveEffect (EffectCtx iid (SourceCard cid) Nothing) (PlaceDoomAt YourSpace (N 1))
            ]
        ]

-- Zoey Samaras's cards -----------------------------------------------------

{- | "You get +2 strength as part of an attack action. After you reroll a die while
resolving a test, add one to the result of that die."
-}
chefsKnife :: AssetBehavior
chefsKnife =
  testBonuses [OnAction AttackAction Strength 2]
    & #afterReroll
    .~ \_ _ idx -> pure [RaiseDie idx]

{- | "After you become engaged with a monster, you may deal one damage to this item to
test will. Deal damage to that monster equal to your test result."
-}
zoeysCross :: AssetBehavior
zoeysCross =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterEngaged iid mid -> do
        name <- (.name) <$> getCardDef mid
        pure
          [ Reaction
              "zoeys-cross"
              ("Zoey's Cross: strike at " <> name)
              [ HarmAsset cid 1 0
              , NoteOnCard cid "target" (coerce mid)
              , BeginTest (newTest iid Will 0 OtherTest (AfterCustom (SourceCard cid) "zoeys-cross"))
              ]
          ]
      _ -> pure []

crossResult :: Source -> Int -> GameM ()
crossResult src r = case src of
  SourceCard cid -> do
    target <- uses #assets (maybe Nothing (Map.lookup "target" . (.tokens)) . Map.lookup cid)
    assetL cid . #tokens .= mempty
    for_ target \n -> do
      let mid = CardId n
      there <- uses #monsters (Map.member mid)
      when (there && r > 0) $ push (DealMonsterDamage mid src r)
  _ -> pure ()

{- | "At the start of your turn, you may test lore. If you pass, attach this card to a
weapon in your space. Attached: Once per round, while performing an attack action,
you may reroll one die or all dice."
-}
enchantWeapon :: AssetBehavior
enchantWeapon =
  defaultAssetBehavior
    & #reactions
    .~ ( \cid -> \case
           AtStartOfTurn iid ->
             pure
               [ Reaction
                   "enchant-weapon"
                   "Enchant Weapon: bind the spell to a weapon"
                   [ castingTest
                       (EffectCtx iid (SourceCard cid) Nothing)
                       cid
                       0
                       (AfterCustom (SourceCard cid) "enchant-weapon")
                   ]
               ]
           _ -> pure []
       )
    & #testOptions
    .~ \cid iid ts -> do
      used <- usedThisRound cid iid
      bound <- uses #assets (maybe False (isJust . (.attachedTo)) . Map.lookup cid)
      let attacking = case ts.kind of ActionTest AttackAction _ -> True; _ -> False
      pure
        [ r
        | bound
        , not used
        , attacking
        , liveDiceCount ts > 0
        , r <-
            [ Reaction
                "enchant-weapon-one"
                "Enchant Weapon: reroll one die"
                [MarkAssetUsed iid cid, RerollUpTo (SourceCard cid) 1]
            , Reaction
                "enchant-weapon-all"
                "Enchant Weapon: reroll all your dice"
                [MarkAssetUsed iid cid, RerollAll (SourceCard cid)]
            ]
        ]

weaponEnchanted :: Source -> Int -> GameM ()
weaponEnchanted src r = case src of
  SourceCard cid -> do
    owner <- uses #assets (fmap (.owner) . Map.lookup cid)
    for_ owner \iid ->
      when (r > 0) $ push (ResolveEffect (EffectCtx iid src Nothing) (Custom "enchant-weapon"))
  _ -> pure ()

bindTheSpell :: EffectCtx -> GameM ()
bindTheSpell ctx = case ctx.source of
  SourceCard cid -> do
    here <- investigatorSpace ctx.investigator
    invs <- playingInvestigators
    let present = [i | isJust here, i <- invs, i.space == here]
    weapons <- concat <$> for present \i -> filterM (cardMatches (WithTrait "Weapon")) i.assets
    named <- for weapons \w -> (w,) . (.name) <$> getCardDef w
    unless (null named)
      $ chooseFor
        ctx.investigator
        "Choose a weapon to enchant"
        [Choice (CardsLabel name [w]) [AttachAsset cid w] | (w, name) <- named]
  _ -> pure ()

-- Patrice Hathaway's Watcher -----------------------------------------------

{- | The Watcher never closes in: the moment it would engage anyone the card turns
over and becomes the condition, and the monster leaves the board.
-}
theWatcher :: MonsterBehavior
theWatcher =
  defaultMonsterBehavior
    & #insteadOfEngaging
    .~ \mid iid -> do
      logText "The Watcher is upon you"
      pure (Just [GainConditionMsg iid "THE WATCHER", DiscardMonster mid])

{- | "While you are resolving a test, you must reroll one success. If you pass that
test, you may spend one clue to discard this card."
-}
theWatcherCondition :: AssetBehavior
theWatcherCondition =
  defaultAssetBehavior
    & #forcedRerollOfSuccess
    .~ True
    & #reactions
    .~ \cid -> \case
      AfterPassedTest iid -> do
        i <- getInvestigator iid
        mine <- uses #assets (maybe False ((== iid) . (.owner)) . Map.lookup cid)
        pure
          [ Reaction
              "the-watcher"
              "The Watcher: spend a clue to be rid of it"
              [ PayCost (EffectCtx iid (SourceCard cid) Nothing) (SpendClues 1)
              , DiscardAsset cid
              ]
          | mine
          , i.clues > 0
          ]
      _ -> pure []

-- Ashcan Pete's card, printed in this box --------------------------------

{- | "Encounter: Once per round, you may choose an adjacent space. Resolve an
encounter as though you are in that space, rolling one fewer die on any tests
during that encounter." Taking it is the encounter, and the card remembers that it
is away from home until the encounter it went looking for has finished.
-}
wanderer :: AssetBehavior
wanderer =
  defaultAssetBehavior
    & #encounterAbilities
    .~ [ ComponentActionDef
           { label = "Wanderer: read an adjacent space instead"
           , allowedWhileEngaged = False
           , canPerform = \iid -> not . null <$> nextDoorDecks iid
           , perform = \ctx -> push (ResolveEffect ctx (Custom "wanderer"))
           }
       ]
    & #poolDelta
    .~ ( \cid _ _ -> do
           away <- uses #assets (maybe False (Map.member "wandering" . (.tokens)) . Map.lookup cid)
           here <- uses #encounter isJust
           pure (if away && here then -1 else 0)
       )
    & #reactions
    .~ \cid -> \case
      -- the road home: whatever it read is done with, so the penalty lifts
      AfterEncounter _ -> do
        assetL cid . #tokens .= mempty
        pure []
      _ -> pure []

{- | The adjacent spaces that have an encounter to read, with the deck each one
would be read from. A special space keeps its own counsel, so it is left out.
-}
nextDoorDecks :: InvestigatorId -> GameM [(SpaceId, Text, EncounterDeck)]
nextDoorDecks iid = do
  msid <- investigatorSpace iid
  board <- use #board
  catMaybes <$> for (maybe [] (`adjacentSpaces` board) msid) \sid -> do
    s <- getSpace sid
    pure $ (sid,s.name,) <$> case s.kind of
      LocationSpace -> NeighborhoodDeck <$> s.neighborhood
      StreetSpace _ -> Just StreetDeck
      TravelRouteSpace _ -> Just TravelRouteDeck
      ThresholdSpace _ -> Just ThresholdDeck
      MysterySpace -> Just (MysteryDeck sid)
      SpecialSpace -> Nothing

wanderOff :: EffectCtx -> GameM ()
wanderOff ctx = case ctx.source of
  SourceCard cid -> do
    options <- nextDoorDecks ctx.investigator
    unless (null options)
      $ chooseFor
        ctx.investigator
        "Choose the space to read"
        [ Choice (SpaceLabel sid) [RememberOnCard cid "wandering", ResolveEncounterFrom ctx.investigator deck]
        | (sid, _, deck) <- options
        ]
  _ -> pure ()

-- | Patrice's optional setup: the dreams come with the thing that sends them.
shuffleInTheWatcher :: EffectCtx -> GameM ()
shuffleInTheWatcher _ = do
  cid <- newCard "the-watcher"
  deck <- use (#decks . #monster)
  #decks . #monster <~ shuffle (cid : deck)
  logText "The Watcher is shuffled into the monster deck"
