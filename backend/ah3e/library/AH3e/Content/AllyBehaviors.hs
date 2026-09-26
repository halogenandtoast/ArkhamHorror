-- | Mechanics for allies whose text the effect vocabulary cannot express.
module AH3e.Content.AllyBehaviors (behaviors) where

import AH3e.Content.Vocabulary (curioItem)
import AH3e.Engine.Behavior
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Board
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
      [ ("alice-luxley", defaultAssetBehavior & #bonusDicePerRound .~ aliceLuxleyDice)
      , ("arthur-johnson", defaultAssetBehavior & #freeRerollPerRound .~ True)
      ,
        ( "delphinia-bell"
        , cardAction
            "Delphinia Bell: spend a remnant to focus a skill"
            (Pay (SpendRemnants 1) (Focus Nothing True))
        )
      ,
        ( "ezra-graves"
        , cardAction
            "Ezra Graves: suffer one direct horror to gain an ally"
            (Pay (SpendRemnants 2) (Seq [DirectHorror (N 1), GainE (AnAlly Nothing)]))
        )
      , ("daniel-chesterfield", danielChesterfield)
      , ("dayana-esperence", dayanaEsperence)
      , ("gabriel-carillo", defaultAssetBehavior & #extraActions .~ 1)
      , ("grace-bechman", testBonuses [OnAction WardAction Lore 2])
      , ("leland-williams", defaultAssetBehavior & #afterGainedFromDeck ?~ curioItem)
      , ("hunting-dog", huntingDog)
      , ("lewis-hayes", testBonuses [WhileCasting 2])
      , ("sachiko-higa", testBonuses [OnAction AttackAction Strength 2])
      ,
        ( "zora-larson"
        , cardAction "Zora Larson: recover one sanity" (RecoverSanity InvestigatorOrAllyInYourSpace (N 1))
        )
      ]

-- | One die per clue you hold, plus one per clue in your neighborhood.
huntingDog :: AssetBehavior
huntingDog =
  defaultAssetBehavior
    & #dieOptions
    .~ \cid iid ts -> do
      used <- usedThisRound cid iid
      clues <- neighborhoodClues iid
      let live = liveDiceCount ts
      pure
        [ Reaction
            "hunting-dog"
            ("Hunting Dog: reroll up to " <> tshow (min live clues) <> " dice")
            [MarkAssetUsed iid cid, RerollUpTo (SourceCard cid) (min live clues)]
        | not used
        , live > 0
        , clues > 0
        ]

-- | One or all, so the two are offered as they are printed, not as any number.
dayanaEsperence :: AssetBehavior
dayanaEsperence =
  defaultAssetBehavior
    & #dieOptions
    .~ \cid iid ts -> do
      used <- usedThisRound cid iid
      let live = liveDiceCount ts
          casting = isJust ts.casting || isSpellTest ts.kind
          offer key lbl ms = Reaction key ("Dayana Esperence: " <> lbl) (MarkAssetUsed iid cid : ms)
      pure
        [ o
        | not used
        , casting
        , live > 0
        , o <-
            [ offer "dayana-esperence-one" "reroll one die" [RerollUpTo (SourceCard cid) 1]
            , offer "dayana-esperence-all" "reroll all dice" [RerollAll (SourceCard cid)]
            ]
        ]

{- | The pact comes due afterwards, so it rides behind the rerolls in the queue
rather than inside them.
-}
danielChesterfield :: AssetBehavior
danielChesterfield =
  defaultAssetBehavior
    & #dieOptions
    .~ \cid iid ts -> do
      used <- usedThisRound cid iid
      let live = liveDiceCount ts
          ctx = EffectCtx iid (SourceCard cid) Nothing
          pact = If (Not (HasCondition "DARK PACT")) (GainE (Condition "DARK PACT")) NoEffect
      pure
        [ Reaction
            "daniel-chesterfield"
            "Daniel Chesterfield: reroll any number of dice, then gain a DARK PACT"
            [MarkAssetUsed iid cid, RerollUpTo (SourceCard cid) live, ResolveEffect ctx pact]
        | not used
        , live > 0
        ]
