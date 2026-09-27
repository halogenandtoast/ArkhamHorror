module AH3e.Content.Core.InvestigatorBehaviors (behaviors) where

import AH3e.Content.Vocabulary
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
    { investigators =
        Map.fromList
          [
            ( "ashcan-pete"
            , afterGatherIf
                (\_ -> not . null <$> scroungeable)
                "scrounge"
                "Scrounge: gain an item worth $2 or less from the display"
                (Custom "scrounge")
            )
          , ("agnes-baker", defaultInvestigatorBehavior & #castWithDamage .~ True & #paidCastLoreBonus .~ 2)
          , ("michael-mcglen", outForRevenge)
          , ("dexter-drake", magicalGift)
          , ("minh-thi-phan", allAroundYou)
          , ("norman-withers", inTheStars)
          , ("rex-murphy", familyCurse)
          , -- Shield From Harm, the sheet's own version of what a card may do
            ("tommy-muldoon", defaultInvestigatorBehavior & #mayTakeEngagement .~ True)
          , ("marie-lambeau", smokyVelvet)
          , ("jenny-barnes", trustFund)
          ,
            ( "daniela-reyes"
            , afterGather "love-for-the-job" "Love For the Job: focus one skill" (Focus Nothing False)
            )
          ]
    , assets =
        Map.fromList
          [
            ( "duke"
            , defaultAssetBehavior
                & #testDice
                .~ (\_ _ ts -> pure (if isStrengthAttack ts then Just 1 else Nothing))
                & #tradeInNeighborhood
                .~ True
            )
          , ("wrench", defaultAssetBehavior & #testDice .~ wrenchDice)
          , ("becky", testBonuses [OnAction AttackAction Strength 4])
          , ("grande-meres-knife", testBonuses [OnAction AttackAction Strength 2, WhileCasting 2])
          , ("jennys-twin-45s", testBonuses [OnAction AttackAction Strength 3])
          , ("magicians-cane", testBonuses [WhileCasting 2])
          , ("storm-of-spirits", defaultAssetBehavior & #attackSkillInstead ?~ Lore)
          , ("spirit-dagger", testBonuses [OnAction AttackAction Strength 2, OnAction WardAction Lore 2])
          , ("gabriel", defaultAssetBehavior & #moveAction ?~ (3, 1))
          , -- the same offer as Gabriel's: three spaces, and a dollar buys a fourth
            ("motorcycle", defaultAssetBehavior & #moveAction ?~ (3, 1))
          , ("chicago-typewriter", testBonuses [OnAction AttackAction Strength 4])
          , ("overcome-all-odds", defaultAssetBehavior & #focusPerSkill .~ 2)
          , ("analytical-mind", analyticalMind)
          , ("mamas-amulet", defaultAssetBehavior & #preventsOneHarmPerRound .~ True)
          , ("ol-boiler", olBoiler)
          , ("handcuffs", handcuffs)
          , ("mr-pawterson", defaultAssetBehavior & #mayStopAttacks .~ True)
          , ("until-the-end-of-time", untilTheEndOfTime)
          , ("it-all-comes-together", itAllComesTogether)
          , ("witch-blood", witchBlood)
          , ("the-tower", rerollOneOrAll "the-tower" "The Tower")
          , ("astronomy-book", astronomyBook)
          , ("search-for-izzie", searchForIzzie)
          , ("voice-of-the-messenger", voiceOfTheMessenger)
          ,
            ( "dressed-to-the-nines"
            , rerollInstead
                "dressed-to-the-nines"
                "Dressed to the Nines: reroll any number of dice instead"
                liveDiceCount
            )
          , ("mysterious-photo", afterClue focusAny)
          , ("search-for-the-truth", afterClue (Seq [money 1, focusAny]))
          , ("precious-memento", preciousMemento)
          , ("king-in-yellow", kingInYellow)
          , ("obannion-member", obannionMember)
          ,
            ( "heirloom-of-hyperborea"
            , defaultAssetBehavior
                & #reactions
                .~ ( \cid -> \case
                       AfterCastSpell iid _ -> do
                         let ctx = EffectCtx iid (SourceCard cid) Nothing
                         ok <- effectUseful ctx (Focus Nothing False)
                         pure
                           [ Reaction
                               "heirloom-of-hyperborea"
                               "Heirloom of Hyperborea: focus one skill"
                               [ResolveEffect ctx (Focus Nothing False)]
                           | ok
                           ]
                       _ -> pure []
                   )
            )
          ,
            ( "petes-guitar"
            , defaultAssetBehavior
                & #reactions
                .~ ( \cid -> \case
                       AfterGatherResources iid -> do
                         let ctx = EffectCtx iid (SourceCard cid) Nothing
                         -- nobody in the neighborhood could recover or focus: nothing to offer
                         who <- guitarCandidates ctx
                         pure
                           [ Reaction "petes-guitar" "Pete's Guitar" [ResolveEffect ctx (Custom "petes-guitar")]
                           | not (null who)
                           ]
                       _ -> pure []
                   )
            )
          ,
            ( "dark-dreams"
            , defaultAssetBehavior
                & #reactions
                .~ ( \cid -> \case
                       DrewBlankToken iid ->
                         pure
                           [ Reaction
                               "dark-dreams"
                               "Dark Dreams: suffer one direct horror to focus a skill and spawn a clue"
                               [ SufferHarm iid (SourceCard cid) DirectHarm 0 1
                               , ResolveEffect (EffectCtx iid (SourceCard cid) Nothing) (Seq [Focus Nothing False, SpawnOneClue])
                               ]
                           ]
                       _ -> pure []
                   )
            )
          ,
            ( "ace-of-swords"
            , defaultAssetBehavior
                & #reactions
                .~ ( \_ -> \case
                       SpentFocusToReroll iid ->
                         pure [Reaction "ace-of-swords" "Ace of Swords: recover one sanity" [RecoverInvestigator iid 0 1]]
                       _ -> pure []
                   )
            )
          ]
    , customEffects =
        Map.fromList
          [ ("scrounge", scrounge)
          , ("petes-guitar", petesGuitar)
          , ("until-the-end-of-time", mendItself)
          ]
    }

afterGather :: Text -> Text -> Effect -> InvestigatorBehavior
afterGather = afterGatherIf (\_ -> pure True)

-- | As 'afterGather', but the reaction is kept back when it has nothing to offer.
afterGatherIf :: (InvestigatorId -> GameM Bool) -> Text -> Text -> Effect -> InvestigatorBehavior
afterGatherIf usable key lbl eff =
  defaultInvestigatorBehavior
    & #reactions
    .~ \self -> \case
      AfterGatherResources iid
        | iid == self -> do
            ok <- usable iid
            pure [Reaction key lbl [ResolveEffect (EffectCtx iid (SourceInvestigator iid) Nothing) eff] | ok]
      _ -> pure []

isStrengthAttack :: TestState -> Bool
isStrengthAttack ts =
  ts.skill == Strength && case ts.kind of
    ActionTest AttackAction _ -> True
    _ -> False

wrenchDice :: CardId -> InvestigatorId -> TestState -> GameM (Maybe Int)
wrenchDice self _ ts
  | not (isStrengthAttack ts) = pure Nothing
  | otherwise = do
      others <- for (filter (/= self) ts.chosenAssets) \c -> maybe 0 (.hands) <$> assetDef c
      pure (Just (if sum others == 0 then 3 else 1))

-- | The display items Pete could take: his ability names $2 or less.
scroungeable :: GameM [CardId]
scroungeable = do
  display <- use (#decks . #display)
  fmap catMaybes $ for display \cid -> do
    md <- assetDef cid
    pure do
      d <- md
      v <- d.value
      guard (v <= 2)
      pure cid

scrounge :: EffectCtx -> GameM ()
scrounge ctx = do
  cheap <- scroungeable
  chooseFor
    ctx.investigator
    "Scrounge"
    ( Choice (DoneLabel "Skip") []
        : [Choice (CardLabel c) [GainFromDisplay ctx.investigator c] | c <- cheap]
    )

guitarChoice :: Effect
guitarChoice =
  Choose [("Recover one sanity", RecoverSanity You (N 1)), ("Focus one skill", Focus Nothing False)]

{- | Investigators in your neighborhood who could lose horror or gain a focus; a
zero influence reaches nobody.
-}
guitarCandidates :: EffectCtx -> GameM [InvestigatorId]
guitarCandidates ctx = do
  n <- skillValue ctx.investigator Influence
  mnid <- investigatorNeighborhood ctx.investigator
  others <- playingInvestigators
  board <- use #board
  let inHood i = isJust mnid && (i.space >>= (`spaceNeighborhood` board)) == mnid
  if n <= 0
    then pure []
    else
      filterM (\i -> effectUseful (ctx & #investigator .~ i) guitarChoice) [i.id | i <- others, inHood i]

petesGuitar :: EffectCtx -> GameM ()
petesGuitar ctx = do
  n <- skillValue ctx.investigator Influence
  candidates <- guitarCandidates ctx
  push (ChooseInvestigatorsFor ctx n candidates guitarChoice)

{- | "After you defeat a monster as part of an attack action, you recover one sanity
or focus one skill of your choice."
-}
outForRevenge :: InvestigatorBehavior
outForRevenge =
  defaultInvestigatorBehavior
    & #reactions
    .~ \self -> \case
      AfterDefeatMonsterInAttack iid | iid == self -> do
        let ctx = EffectCtx iid (SourceInvestigator iid) Nothing
            choice = Choose [("Recover one sanity", mySanity 1), ("Focus one skill", focusAny)]
        pure [Reaction "out-for-revenge" "Out for Revenge" [ResolveEffect ctx choice]]
      _ -> pure []

{- | "Action: If you have fewer than $3, you gain $3." Dressed to the Nines adds two
more to it, which is why the gain is counted here rather than written as an effect.
-}
trustFund :: InvestigatorBehavior
trustFund =
  defaultInvestigatorBehavior
    & #componentActions
    .~ [ ComponentActionDef
           { label = "Trust Fund: gain $3"
           , allowedWhileEngaged = True
           , canPerform = \iid -> (< 3) . (.money) <$> getInvestigator iid
           , perform = \ctx -> do
               dressed <- holdsCard ctx.investigator "dressed-to-the-nines"
               addMoney ctx.investigator (if dressed then 5 else 3)
           }
       ]

-- | "Once per round, while resolving a test, you may reroll one or all of your dice."
rerollOneOrAll :: Text -> Text -> AssetBehavior
rerollOneOrAll key name =
  defaultAssetBehavior
    & #testOptions
    .~ \cid iid ts -> do
      used <- usedThisRound cid iid
      let live = liveDiceCount ts
          offer k lbl ms = Reaction (key <> k) (name <> ": " <> lbl) (MarkAssetUsed iid cid : ms)
      pure
        [ o
        | not used
        , live > 0
        , o <-
            [ offer "-one" "reroll one die" [RerollUpTo (SourceCard cid) 1]
            , offer "-all" "reroll all dice" [RerollAll (SourceCard cid)]
            ]
        ]

{- | "Once per round, while resolving a test, you may reroll a number of dice up to
the amount of doom in your space."
-}
astronomyBook :: AssetBehavior
astronomyBook =
  defaultAssetBehavior
    & #testOptions
    .~ \cid iid ts -> do
      used <- usedThisRound cid iid
      doom <- investigatorSpace iid >>= maybe (pure 0) (fmap (.doom) . getSpace)
      let live = liveDiceCount ts
      pure
        [ Reaction
            "astronomy-book"
            ("Astronomy Book: reroll up to " <> tshow (min live doom) <> " dice")
            [MarkAssetUsed iid cid, RerollUpTo (SourceCard cid) (min live doom)]
        | not used
        , live > 0
        , doom > 0
        ]

{- | "Once per round, while resolving a test, you may suffer one damage and one horror
to reroll any number of dice."
-}
searchForIzzie :: AssetBehavior
searchForIzzie =
  defaultAssetBehavior
    & #testOptions
    .~ \cid iid ts -> do
      used <- usedThisRound cid iid
      let live = liveDiceCount ts
      pure
        [ Reaction
            "search-for-izzie"
            "Search for Izzie: suffer one damage and one horror to reroll any number of dice"
            [ MarkAssetUsed iid cid
            , SufferHarm iid (SourceCard cid) NormalHarm 1 1
            , RerollUpTo (SourceCard cid) live
            ]
        | not used
        , live > 0
        ]

{- | "You may suffer one horror to reroll any number of dice." Once per roll rather
than once per round, so it is the test that remembers it.
-}
voiceOfTheMessenger :: AssetBehavior
voiceOfTheMessenger =
  defaultAssetBehavior
    & #testOptions
    .~ \cid iid ts -> do
      let live = liveDiceCount ts
      pure
        [ Reaction
            "voice-of-the-messenger"
            "Voice of the Messenger: suffer one horror to reroll any number of dice"
            [ MarkUsedInTest cid
            , SufferHarm iid (SourceCard cid) NormalHarm 0 1
            , RerollUpTo (SourceCard cid) live
            ]
        | cid `notElem` ts.usedInTest
        , live > 0
        ]

-- | A card that answers a clue its owner gained, without being asked.
afterClue :: Effect -> AssetBehavior
afterClue eff =
  defaultAssetBehavior
    & #afterGainClue
    .~ \cid iid -> pure [ResolveEffect (EffectCtx iid (SourceCard cid) Nothing) eff]

-- | "After you gain a clue, remove two horror from this card."
preciousMemento :: AssetBehavior
preciousMemento =
  defaultAssetBehavior
    & #afterGainClue
    .~ \cid _ -> pure [RecoverAsset cid 0 2]

{- | "Once per round, after you suffer one or more horror, you may research one clue."
Researching moves a clue of your own, so it needs one to move.
-}
kingInYellow :: AssetBehavior
kingInYellow =
  defaultAssetBehavior
    & #afterHarm
    .~ \cid iid plan -> do
      used <- usedThisRound cid iid
      i <- getInvestigator iid
      pure
        [ AskAboutAsset
            iid
            cid
            "King in Yellow: research one clue?"
            [ Choice (DoneLabel "Skip") []
            , Choice (TextLabel "Research one clue") [MarkAssetUsed iid cid, ResearchCluesExact iid 1]
            ]
        | not used
        , plan.horror > 0
        , i.clues > 0
        ]

-- | "After you perform a gather resources action, you may test strength. If you pass, you gain $2."
obannionMember :: AssetBehavior
obannionMember =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterGatherResources iid ->
        pure
          [ Reaction
              "obannion-member"
              "O'Bannion Member: test strength for $2"
              [ResolveEffect (EffectCtx iid (SourceCard cid) Nothing) (pass Strength 0 (money 2))]
          ]
      _ -> pure []

{- | "Once per round, while resolving a lore test, you may reroll one or all of your
dice." His focus limit, the other half of the sheet, is counted from his spells (see
'AH3e.Content.Core.Investigators.focusLimitFromSpells').
-}
magicalGift :: InvestigatorBehavior
magicalGift =
  defaultInvestigatorBehavior
    & #testOptions
    .~ \iid ts -> do
      used <- usedAbility iid "magical-gift"
      let live = liveDiceCount ts
          offer k lbl ms = Reaction k ("Magical Gift: " <> lbl) (MarkAbilityUsed iid "magical-gift" : ms)
      pure
        [ o
        | not used
        , live > 0
        , ts.skill == Lore
        , o <-
            [ offer "magical-gift-one" "reroll one die" [RerollUpTo (SourceInvestigator iid) 1]
            , offer "magical-gift-all" "reroll all dice" [RerollAll (SourceInvestigator iid)]
            ]
        ]

{- | "Once per round, while resolving a test, you or another investigator in your space
may reroll dice up to the number of clues in your neighborhood." Her own test is
offered through the sheet; someone else's reaches her through the window their roll
opens, since it is her ability to spend.
-}
allAroundYou :: InvestigatorBehavior
allAroundYou =
  defaultInvestigatorBehavior
    & #testOptions
    .~ minhReroll
    & #reactions
    .~ \self -> \case
      AnotherResolvesTest owner tested | owner == self -> do
        mine <- investigatorSpace self
        theirs <- investigatorSpace tested
        mts <- use #test
        case mts of
          Just ts | isJust mine, mine == theirs -> minhReroll self ts
          _ -> pure []
      _ -> pure []

-- | The offer itself: as many dice as there are clues in her neighborhood.
minhReroll :: InvestigatorId -> TestState -> GameM [Reaction]
minhReroll iid ts = do
  used <- usedAbility iid "all-around-you"
  clues <- neighborhoodClues iid
  let live = liveDiceCount ts
  pure
    [ Reaction
        "all-around-you"
        ("All Around You: reroll up to " <> tshow (min live clues) <> " dice")
        [MarkAbilityUsed iid "all-around-you", RerollUpTo (SourceInvestigator iid) (min live clues)]
    | not used
    , live > 0
    , clues > 0
    ]

{- | "You roll one additional die while resolving a will or observation test if you are
at or above your focus limit."
-}
analyticalMind :: AssetBehavior
analyticalMind =
  defaultAssetBehavior
    & #testDice
    .~ \_ iid ts ->
      if ts.skill `notElem` [Will, Observation]
        then pure Nothing
        else do
          i <- getInvestigator iid
          limit <- focusLimit iid
          pure (if maybe False (focusCount i >=) limit then Just 1 else Nothing)

{- | "After you remove two or more doom from your space, you may suffer one horror to
research one clue." Researching moves a clue of your own, so it needs one to move.
-}
inTheStars :: InvestigatorBehavior
inTheStars =
  defaultInvestigatorBehavior
    & #reactions
    .~ \self -> \case
      AfterDoomRemoved iid removed
        | iid == self
        , removed >= 2 -> do
            i <- getInvestigator iid
            pure
              [ Reaction
                  "in-the-stars"
                  "In the Stars: suffer one horror to research one clue"
                  [SufferHarm iid (SourceInvestigator iid) NormalHarm 0 1, ResearchCluesExact iid 1]
              | i.clues > 0
              ]
      _ -> pure []

{- | "Family Curse -- While resolving a test, only 6s count as successes. You cannot
become BLESSED or CURSED. Never Give Up -- After you fail a test, you focus one skill
of your choice."
-}
familyCurse :: InvestigatorBehavior
familyCurse =
  defaultInvestigatorBehavior
    & #successOnSix
    .~ True
    & #bansConditions
    .~ ["BLESSED", "CURSED"]
    & #reactions
    .~ \self -> \case
      AfterFailedTest iid | iid == self -> do
        let ctx = EffectCtx iid (SourceInvestigator iid) Nothing
        ok <- effectUseful ctx focusAny
        pure [Reaction "never-give-up" "Never Give Up: focus one skill" [ResolveEffect ctx focusAny] | ok]
      _ -> pure []

{- | "After you perform a move action, you may deal two damage to one monster you are
engaged with and two damage to this card." The boiler takes its own two whether the
monster survives or not.
-}
olBoiler :: AssetBehavior
olBoiler =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterMoveAction iid -> do
        engaged <- engagedMonsters iid
        pure
          [ Reaction
              "ol-boiler"
              "Ol' Boiler: two damage to a monster and two to itself"
              [ HarmAsset cid 2 0
              , ResolveEffect
                  (EffectCtx iid (SourceCard cid) Nothing)
                  (DamageMonsterIn YourSpace (N 2))
              ]
          | not (null engaged)
          ]
      _ -> pure []

{- | "Once per round, after you damage, disengage, or are damaged by a non-epic human
monster, you may defeat that monster." All three ways in are covered: what its owner
damages, what damages them, and what comes apart from them.
-}
handcuffs :: AssetBehavior
handcuffs =
  defaultAssetBehavior
    & #afterMonsterDamaged
    .~ ( \cid owner mid src -> case src of
           SourceInvestigator who | who == owner -> cuffOffer cid owner mid
           _ -> pure []
       )
    & #afterHarm
    .~ ( \cid iid plan -> case plan.source of
           SourceMonster mid | plan.damage > 0 -> cuffOffer cid iid mid
           _ -> pure []
       )
    & #reactions
    .~ \cid -> \case
      AfterDisengage iid mid -> cuffOffer cid iid mid >>= \ms -> pure [Reaction "handcuffs" "Handcuffs" ms | not (null ms)]
      _ -> pure []

-- | The offer itself, for a human monster that is not epic and still in play.
cuffOffer :: CardId -> InvestigatorId -> CardId -> GameM [Message]
cuffOffer cid iid mid = do
  used <- usedThisRound cid iid
  here <- uses #monsters (Map.member mid)
  d <- monsterDef mid
  name <- (.name) <$> getCardDef mid
  pure
    [ AskAboutAsset
        iid
        cid
        ("Handcuffs: defeat " <> name <> "?")
        [ Choice (DoneLabel "Skip") []
        , Choice (TextLabel ("Defeat " <> name)) [MarkAssetUsed iid cid, DefeatMonster mid (SourceCard cid)]
        ]
    | not used
    , here
    , not d.epic
    , "Human" `elem` d.traits
    ]

{- | "This card cannot be discarded by any means. Reckoning -- Remove one damage and
one horror from this card."
-}
untilTheEndOfTime :: AssetBehavior
untilTheEndOfTime =
  defaultAssetBehavior
    & #undiscardable
    .~ True
    & #reckoning
    ?~ Custom "until-the-end-of-time"

-- | The card mends itself, which needs the card rather than its owner.
mendItself :: EffectCtx -> GameM ()
mendItself ctx = for_ [cid | SourceCard cid <- [ctx.source]] \cid -> push (RecoverAsset cid 1 1)

-- | "As part of a research action, add one to the result of each die you roll."
itAllComesTogether :: AssetBehavior
itAllComesTogether =
  defaultAssetBehavior
    & #dieBonus
    .~ \_ _ ts -> pure case ts.kind of
      ActionTest ResearchAction _ -> 1
      _ -> 0

{- | "Once per round, after you perform an action, another investigator on any space
may perform that same action." What they may do is still their own business, so the
offer only reaches those the action is legal for.
-}
smokyVelvet :: InvestigatorBehavior
smokyVelvet =
  defaultInvestigatorBehavior
    & #reactions
    .~ \self -> \case
      AfterAnyAction iid kind | iid == self -> do
        used <- usedAbility self "smoky-velvet"
        pure
          [ Reaction
              "smoky-velvet"
              "Smoky Velvet: another investigator may take that action"
              [MarkAbilityUsed self "smoky-velvet", OfferGrantedAction self kind]
          | not used
          ]
      _ -> pure []

{- | "Action: You may perform an action you have already performed this round. ... Once
per round, after you spend a remnant, you gain one remnant." Its first half reaches
Smoky Velvet by itself, since a granted action is performed like any other.
-}
witchBlood :: AssetBehavior
witchBlood =
  defaultAssetBehavior
    & #componentActions
    .~ [ ComponentActionDef
           { label = "Witch Blood: take an action again"
           , allowedWhileEngaged = True
           , canPerform = \iid -> not . null . repeatable <$> getInvestigator iid
           , perform = \ctx -> do
               i <- getInvestigator ctx.investigator
               chooseFor ctx.investigator "Take which action again?"
                 $ [ Choice (ActionLabel k) [PerformGrantedAction ctx.investigator k True]
                   | k <- repeatable i
                   ]
           }
       ]
    & #reactions
    .~ \cid -> \case
      AfterSpendRemnant iid -> do
        used <- usedThisRound cid iid
        pure
          [ Reaction
              "witch-blood"
              "Witch Blood: gain one remnant"
              [MarkAssetUsed iid cid, ResolveEffect (EffectCtx iid (SourceCard cid) Nothing) (remnants 1)]
          | not used
          ]
      _ -> pure []

{- | The actions an investigator may take again: the ones they have taken, bar the card
actions, which are the cards' own business.
-}
repeatable :: Investigator -> [ActionKind]
repeatable i = [k | k <- i.performed, not (isComponent k)]
 where
  isComponent = \case ComponentAction _ _ -> True; _ -> False
