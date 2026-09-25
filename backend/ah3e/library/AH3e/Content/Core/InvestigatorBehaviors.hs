module AH3e.Content.Core.InvestigatorBehaviors (behaviors) where

import AH3e.Engine.Behavior
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
          , ("38-revolver", testBonuses [OnAction AttackAction Strength 2])
          , ("45-automatic", testBonuses [OnAction AttackAction Strength 3])
          , ("45-thompson", testBonuses [OnAction AttackAction Strength 5])
          , ("becky", testBonuses [OnAction AttackAction Strength 4])
          , ("grande-meres-knife", testBonuses [OnAction AttackAction Strength 2, WhileCasting 2])
          , ("jennys-twin-45s", testBonuses [OnAction AttackAction Strength 3])
          , ("knife", testBonuses [OnAction AttackAction Strength 1])
          , ("leather-coat", testBonuses [OnAction EvadeAction Observation 1])
          , ("magicians-cane", testBonuses [WhileCasting 2])
          , ("magnifying-glass", testBonuses [OnAction ResearchAction Observation 1])
          , ("mystic-scroll", testBonuses [WhileCasting 2])
          , ("mystic-tome", testBonuses [WhileCasting 3])
          , ("occult-scripture", testBonuses [OnAction ResearchAction Observation 2])
          , ("storm-of-spirits", defaultAssetBehavior & #attackSkillInstead ?~ Lore)
          , ("otherworld-codex", testBonuses [OnAction WardAction Lore 3])
          , ("secret-page", testBonuses [OnAction WardAction Lore 2])
          , ("service-piece", testBonuses [OnAction AttackAction Strength 2])
          , ("shotgun", testBonuses [OnAction AttackAction Strength 5])
          , ("spirit-dagger", testBonuses [OnAction AttackAction Strength 2, OnAction WardAction Lore 2])
          , ("gabriel", defaultAssetBehavior & #moveAction ?~ (3, 1))
          , ("rabbits-foot", defaultAssetBehavior & #freeRerollPerRound .~ True)
          , ("pocket-watch", defaultAssetBehavior & #extraActions .~ 1)
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

{- | When a test asset adds dice: "+N skill as part of an X action", or "+N lore
while casting a spell".
-}
data TestBonus = OnAction ActionKind Skill Int | WhileCasting Int

{- | A test asset whose bonuses add up for the tests they match; it is chosen like
any other test asset and takes its printed hands.
-}
testBonuses :: [TestBonus] -> AssetBehavior
testBonuses bonuses =
  defaultAssetBehavior
    & #testDice
    .~ \_ _ ts -> pure case [n | b <- bonuses, Just n <- [applies ts b]] of
      [] -> Nothing
      ns -> Just (sum ns)
 where
  applies ts = \case
    OnAction action skill n | ActionTest a _ <- ts.kind, a == action, ts.skill == skill -> Just n
    WhileCasting n | isJust ts.casting || isSpellTest ts.kind, ts.skill == Lore -> Just n
    _ -> Nothing

isSpellTest :: TestKind -> Bool
isSpellTest = \case
  SpellTest _ -> True
  _ -> False

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
