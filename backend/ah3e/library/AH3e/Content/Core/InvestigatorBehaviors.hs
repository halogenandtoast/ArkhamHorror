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
    , customEffects = Map.fromList [("scrounge", scrounge), ("petes-guitar", petesGuitar)]
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
