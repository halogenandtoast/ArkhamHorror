-- | Mechanics for spells whose text the effect vocabulary cannot express.
module AH3e.Content.SpellBehaviors (behaviors) where

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
    & #assets
    .~ Map.fromList
      [
        ( "alchemical-process"
        , spellAction "Alchemical Process: gain $1 for each success" 0 (GainE (Money TestResult))
        )
      , ("astral-travel", defaultAssetBehavior & #moveBySpell ?~ (Lore, 0, 2))
      , ("binding", binding)
      , ("clairvoyance", clairvoyance)
      , ("lure-monster", lureMonster)
      ,
        ( "find-gate"
        , spellAction "Find Gate: move to a space with doom" 0 (MoveDirectlyTo AnySpaceWithDoom)
        )
      , ("flesh-ward", fleshWard)
      , ("intervene", intervene)
      , ("mists-of-rlyeh", defaultAssetBehavior & #evadeSkillInstead ?~ Lore)
      , ("wither", wither)
      ,
        ( "healing-words"
        , spellAction
            "Healing Words: recover health"
            (-1)
            (RecoverHealth InvestigatorOrAllyInYourSpace TestResult)
        )
      ,
        ( "shriveling"
        , whileEngaged
            (spellAction "Shriveling: damage a monster" (-1) (DamageMonsterIn YourSpaceOrAdjacent TestResult))
        )
      , ("instill-bravery", instillBravery)
      ,
        ( "wrack"
        , whileEngaged
            (spellAction "Wrack: defeat a monster" 1 (DefeatMonsterIn YourSpace TestResult))
        )
      , -- Secrets of the Order
        ("banishment", banishment)
      , ("the-beast-within", theBeastWithin)
      ]
    & #customAfterTests
    .~ Map.fromList
      [ ("instill-bravery", \_ r -> #horrorPrevented += r)
      , ("clairvoyance", clairvoyanceResult)
      , ("lure-monster", lureResult)
      , ("banishment", banishmentResult)
      , ("the-beast-within", theBeastWithinResult)
      ]
    & #customEffects
    .~ Map.fromList
      [ ("clairvoyance-peek", clairvoyancePeek)
      , ("clairvoyance-discard", clairvoyanceDiscard)
      ]

{- | The monster is chosen before the test, because the test uses that monster's
own evade modifier. Exhausting it drops its engagement with it, since a monster's
state holds either.
-}
binding :: AssetBehavior
binding =
  defaultAssetBehavior
    & #componentActions
    .~ [ ComponentActionDef
           { label = "Binding: exhaust a monster"
           , allowedWhileEngaged = False
           , canPerform = \_ -> not . null <$> uses #monsters Map.elems
           , perform = \ctx -> case ctx.source of
               SourceCard cid -> do
                 ms <- uses #monsters Map.elems
                 choices <- for ms \m -> do
                   d <- monsterDef m.card
                   pure
                     (Choice (MonsterLabel m.card) [castingTest ctx cid d.evadeModifier (AfterExhaustMonster m.card)])
                 chooseFor ctx.investigator "Choose a monster to bind" choices
               _ -> pure ()
           }
       ]

{- | Once per round, cast to prevent damage equal to a lore test result. The
damage may be anyone's, so 'damagePrevention' offers it wherever the sufferer
is; marking the card used before the cast keeps the cast's own damage, which
Agnes may pay, from offering it again.
-}
fleshWard :: AssetBehavior
fleshWard =
  defaultAssetBehavior
    & #damagePrevention
    .~ \cid owner plan ->
      pure
        [ Reaction
            ("flesh-ward-" <> tshow cid)
            "Flesh Ward: test lore to prevent damage"
            [ MarkAssetUsed owner cid
            , CastSpell
                owner
                cid
                [BeginTest (newTest owner Lore 0 (SpellTest cid) AfterPreventDamage) {casting = Just cid}]
            ]
        | plan.damage > 0
        ]

{- | Cast as part of an attack action: its own lore test interrupts the attack's,
and its result is added to the attack's when it finishes.
-}
wither :: AssetBehavior
wither =
  defaultAssetBehavior
    & #testOptions
    .~ \cid iid ts ->
      pure
        [ Reaction
            "wither"
            "Wither: test lore and add the result to this attack"
            [ MarkUsedInTest cid
            , castingTest (EffectCtx iid (SourceCard cid) Nothing) cid (-1) AfterBoostTest
            ]
        | cid `notElem` ts.usedInTest
        , ActionTest AttackAction _ <- [ts.kind]
        ]

{- | Offered to its owner while somebody else is resolving a test, so the owner
decides whether to pay for it. Their result is added to the test they interrupted.
-}
intervene :: AssetBehavior
intervene =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AnotherResolvesTest owner _ -> do
        used <- usedThisRound cid owner
        pure
          [ Reaction
              "intervene"
              "Intervene: test lore and add the result to their test"
              [ MarkAssetUsed owner cid
              , castingTest (EffectCtx owner (SourceCard cid) Nothing) cid (-1) AfterBoostTest
              ]
          | not used
          ]
      _ -> pure []

{- | Once per round, cast to prevent horror equal to a lore test result. The horror
may be anyone's, so 'damagePrevention' offers it wherever the sufferer is.
'AfterPreventDamage' would bank the result as damage, so the result is carried to
'horrorPrevented' through a continuation of this card's own. Marking the card used
before the cast keeps the horror the cast itself costs from offering it again.
-}
instillBravery :: AssetBehavior
instillBravery =
  defaultAssetBehavior
    & #damagePrevention
    .~ \cid owner plan ->
      pure
        [ Reaction
            ("instill-bravery-" <> tshow cid)
            "Instill Bravery: test lore to prevent horror"
            [ MarkAssetUsed owner cid
            , castingTest
                (EffectCtx owner (SourceCard cid) Nothing)
                cid
                0
                (AfterCustom (SourceCard cid) "instill-bravery")
            ]
        | plan.horror > 0
        ]

{- | "Once per round, when a ready, non-epic monster would activate, you may test
lore. If you pass, move it two spaces toward you instead." The monster is noted on
the card, since which one it was is only wanted once the test has answered; a
failed test lets it activate after all.
-}
lureMonster :: AssetBehavior
lureMonster =
  defaultAssetBehavior
    & #replacesActivation
    .~ \cid iid mid -> do
      used <- usedThisRound cid iid
      d <- monsterDef mid
      m <- getMonster mid
      name <- (.name) <$> getCardDef mid
      pure
        [ Reaction
            "lure-monster"
            ("Lure Monster: test lore to draw " <> name <> " two spaces toward you")
            [ MarkAssetUsed iid cid
            , NoteOnCard cid "lured" (coerce mid)
            , castingTest
                (EffectCtx iid (SourceCard cid) Nothing)
                cid
                0
                (AfterCustom (SourceCard cid) "lure-monster")
            ]
        | not used
        , not d.epic
        , m.state == Ready
        ]

-- | The lured monster comes two spaces closer, or activates as it meant to.
lureResult :: Source -> Int -> GameM ()
lureResult src r = for_ [cid | SourceCard cid <- [src]] \cid -> do
  ma <- use (#assets . at cid)
  for_ ma \a -> do
    let lured = coerce <$> Map.lookup "lured" a.tokens
    assetL cid . #tokens .= mempty
    for_ lured \mid -> do
      present <- uses #monsters (Map.member mid)
      here <- investigatorSpace a.owner
      when present case (r > 0, here) of
        (True, Just sid) -> push (MonsterStep mid 2 (TowardSpaces (NamedSpace sid)))
        _ -> push (DoActivateMonster mid)

{- | "At the start of your turn, you may test lore. Look at the top card of a
number of neighborhood decks up to your test result. Of those, you may discard one
non-event card."
-}
clairvoyance :: AssetBehavior
clairvoyance =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AtStartOfTurn iid -> do
        decks <- uses (#decks . #neighborhoods) (filter (not . null) . Map.elems)
        pure
          [ Reaction
              "clairvoyance"
              "Clairvoyance: test lore to look at neighborhood decks"
              [ castingTest
                  (EffectCtx iid (SourceCard cid) Nothing)
                  cid
                  0
                  (AfterCustom (SourceCard cid) "clairvoyance")
              ]
          | not (null decks)
          ]
      _ -> pure []

{- | The result is how many decks may be looked at, which the card counts down as
they are chosen; what has been looked at is remembered on the card as well, so the
discard at the end is offered from those cards alone.
-}
clairvoyanceResult :: Source -> Int -> GameM ()
clairvoyanceResult src r = for_ [cid | SourceCard cid <- [src]] \cid -> do
  ma <- use (#assets . at cid)
  for_ ma \a -> when (r > 0) do
    assetL cid . #tokens .= Map.singleton "left" r
    clairvoyancePeek (EffectCtx a.owner (SourceCard cid) Nothing)

-- | One deck at a time, until the looks run out or they stop.
clairvoyancePeek :: EffectCtx -> GameM ()
clairvoyancePeek ctx = for_ [cid | SourceCard cid <- [ctx.source]] \cid -> do
  ma <- use (#assets . at cid)
  for_ ma \a -> do
    decks <- use (#decks . #neighborhoods)
    let left = Map.findWithDefault 0 "left" a.tokens
        seen nid = Map.member ("seen-" <> coerce nid) a.tokens
        unseen = [(nid, deck) | (nid, deck) <- Map.toList decks, not (null deck), not (seen nid)]
    -- the cards just looked at are read out as they are chosen
    for_ (Map.toList decks) \(nid, deck) ->
      when (seen nid && not (Map.member ("read-" <> coerce nid) a.tokens)) do
        assetL cid . #tokens . at ("read-" <> coerce nid) ?= 1
        for_ (take 1 deck) \top -> do
          name <- (.name) <$> getCardDef top
          logText ("Clairvoyance: the top of the " <> coerce nid <> " deck is " <> name)
    if left <= 0 || null unseen
      then clairvoyanceOfferDiscard ctx cid
      else
        chooseFor ctx.investigator "Look at the top card of a neighborhood deck"
          $ Choice
            (DoneLabel "Stop looking")
            [NoteOnCard cid "left" 0, ResolveEffect ctx (Custom "clairvoyance-peek")]
          : [ Choice
                (TextLabel (coerce nid))
                [ NoteOnCard cid ("seen-" <> coerce nid) 1
                , NoteOnCard cid "left" (left - 1)
                , ResolveEffect ctx (Custom "clairvoyance-peek")
                ]
            | (nid, _) <- unseen
            ]

{- | One of the cards looked at may go, so long as it is not an event; an event
card sits in the deck it was shuffled into and stays there.
-}
clairvoyanceOfferDiscard :: EffectCtx -> CardId -> GameM ()
clairvoyanceOfferDiscard ctx cid = do
  ma <- use (#assets . at cid)
  for_ ma \a -> do
    decks <- use (#decks . #neighborhoods)
    let looked = [top | (nid, top : _) <- Map.toList decks, Map.member ("seen-" <> coerce nid) a.tokens]
    discardable <- filterM (fmap (not . isEvent) . getCardDef) looked
    chooseFor ctx.investigator "Discard one of the cards you looked at?"
      $ Choice (DoneLabel "Discard nothing") [ResolveEffect ctx (Custom "clairvoyance-discard")]
      : [ Choice
            (CardLabel top)
            [NoteOnCard cid "discard" (coerce top), ResolveEffect ctx (Custom "clairvoyance-discard")]
        | top <- discardable
        ]
 where
  isEvent d = case d.kind of EventCard _ -> True; _ -> False

-- | The card chosen, if any, leaves its deck for good; the notes go either way.
clairvoyanceDiscard :: EffectCtx -> GameM ()
clairvoyanceDiscard ctx = for_ [cid | SourceCard cid <- [ctx.source]] \cid -> do
  ma <- use (#assets . at cid)
  for_ ma \a -> do
    assetL cid . #tokens .= mempty
    for_ (Map.lookup "discard" a.tokens) \noted -> do
      let target = coerce noted
      name <- (.name) <$> getCardDef target
      #decks . #neighborhoods %= Map.map (filter (/= target))
      #decks . #removed %= (target :)
      logText ("Clairvoyance discards " <> name)

-- Secrets of the Order --------------------------------------------------------

{- | "Once per round, during your turn, you may choose a non-epic monster and test
lore -1. If you pass, that monster disengages all investigators and moves directly
to the unstable space." The monster is chosen before the test, so it is noted on the
card and read back once the test has answered.
-}
banishment :: AssetBehavior
banishment =
  defaultAssetBehavior
    & #freeActions
    .~ [ ComponentActionDef
           { label = "Banishment: test lore to banish a monster"
           , allowedWhileEngaged = True
           , canPerform = \iid -> do
               used <- usedAbility iid "banishment"
               ms <- nonEpicMonsters
               pure (not used && not (null ms))
           , perform = \ctx -> for_ [c | SourceCard c <- [ctx.source]] \cid -> do
               spendOncePerRound ctx.investigator "banishment"
               ms <- nonEpicMonsters
               chooseFor ctx.investigator "Choose a monster to banish"
                 $ [ Choice
                       (MonsterLabel m.card)
                       [ NoteOnCard cid "banished" (coerce m.card)
                       , castingTest ctx cid (-1) (AfterCustom (SourceCard cid) "banishment")
                       ]
                   | m <- ms
                   ]
           }
       ]

nonEpicMonsters :: GameM [Monster]
nonEpicMonsters = uses #monsters Map.elems >>= filterM (fmap (not . (.epic)) . monsterDef . (.card))

{- | Letting go of everyone and then arriving in the unstable space, in that order:
the monster engages whoever is standing there as it lands, like any other arrival.
-}
banishmentResult :: Source -> Int -> GameM ()
banishmentResult src r = for_ [c | SourceCard c <- [src]] \self -> do
  a <- use (assetL self)
  mid <-
    uses #assets (coerce . Map.findWithDefault 0 "banished" . maybe mempty (.tokens) . Map.lookup self)
  assetL self . #tokens .= mempty
  m <- uses #monsters (Map.lookup mid)
  targets <- unstableSpaces
  for_ m \monster -> when (r > 0) do
    let holders = case monster.state of Engaged is -> is; _ -> []
        letGo = [DisengageMonster who mid | who <- holders]
    case targets of
      [sid] -> pushAll (letGo <> [MoveMonsterTo mid sid])
      _ ->
        chooseFor a.owner "Choose the unstable space"
          $ [Choice (SpaceLabel sid) (letGo <> [MoveMonsterTo mid sid]) | sid <- targets]

{- | "When you perform an attack action, you may test lore. If you pass, roll five
dice instead of your usual dice pool. Ignore all other modifiers." Offered before
the attack's target is chosen, so the pool it states is waiting when that test
begins.
-}
theBeastWithin :: AssetBehavior
theBeastWithin =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      BeforePerformAction iid AttackAction ->
        pure
          [ Reaction
              "the-beast-within"
              "The Beast Within: test lore to roll five dice instead"
              [ castingTest
                  (EffectCtx iid (SourceCard cid) Nothing)
                  cid
                  0
                  (AfterCustom (SourceCard cid) "the-beast-within")
              ]
          ]
      _ -> pure []

theBeastWithinResult :: Source -> Int -> GameM ()
theBeastWithinResult src r = for_ [c | SourceCard c <- [src]] \self -> do
  a <- use (assetL self)
  when (r > 0) $ investigatorL a.owner . #fixedPoolNext ?= 5
