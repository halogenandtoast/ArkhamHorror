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
      , ("black-cat", blackCat)
      , ("gabriel-carillo", defaultAssetBehavior & #extraActions .~ 1)
      , ("grace-bechman", testBonuses [OnAction WardAction Lore 2])
      , ("leland-williams", defaultAssetBehavior & #afterGainedFromDeck ?~ curioItem)
      , ("hunting-dog", huntingDog)
      , ("henry-wan", defaultAssetBehavior & #halfPricePerRound .~ True)
      , ("jenica-capra", jenicaCapra)
      , ("lewis-hayes", testBonuses [WhileCasting 2])
      , ("lita-chantler", litaChantler)
      , ("stray-cat", strayCat)
      , ("stray-dog", strayDog)
      , ("tetsuo-mori", tetsuoMori)
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
    & #testOptions
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
    & #testOptions
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
    & #testOptions
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

-- | One die per clue you hold, plus one per clue in your neighborhood.
aliceLuxleyDice :: CardId -> InvestigatorId -> GameM Int
aliceLuxleyDice _ iid = do
  i <- getInvestigator iid
  here <- neighborhoodClues iid
  pure (i.clues + here)

{- | The horror a spell costs is paid before its test exists, so the success is
promised and the test picks it up as it begins.
-}
blackCat :: AssetBehavior
blackCat =
  defaultAssetBehavior
    & #afterHarm
    .~ \cid _ plan -> do
      casting <- sourceIsSpell plan.source
      pure [AddTestSuccesses 1 | casting, Just (self, k) <- [plan.horrorTo], self == cid, k > 0]

-- | Whether the harm was the cost of casting a spell.
sourceIsSpell :: Source -> GameM Bool
sourceIsSpell = \case
  SourceCard cid -> maybe False ((== Spell) . (.assetType)) <$> assetDef cid
  _ -> pure False

{- | Bites back for damage the monster dealt to its owner or to itself. Its own
bite comes from the card, not from an investigator, so nothing bites back at it.
-}
strayDog :: AssetBehavior
strayDog =
  defaultAssetBehavior
    & #afterHarm
    .~ \cid _ plan -> do
      let onDog = or [self == cid && k > 0 | Just (self, k) <- [plan.damageTo]]
          onOwner = plan.damage - maybe 0 snd plan.damageTo > 0
      pure
        [ DealMonsterDamage mid (SourceCard cid) 1
        | onDog || onOwner
        , SourceMonster mid <- [plan.source]
        ]

{- | "After you defeat a monster as part of an attack action, you may research one
clue." Researching moves a clue of your own onto the sheet, so it needs one.
-}
tetsuoMori :: AssetBehavior
tetsuoMori =
  defaultAssetBehavior
    & #reactions
    .~ \_ -> \case
      AfterDefeatMonsterInAttack iid -> do
        i <- getInvestigator iid
        pure
          [ Reaction "tetsuo-mori" "Tetsuo Mori: research one clue" [ResearchCluesExact iid 1]
          | i.clues > 0
          ]
      _ -> pure []

-- | As Tetsuo, but the doom has to be there to remove.
jenicaCapra :: AssetBehavior
jenicaCapra =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterDefeatMonsterInAttack iid -> do
        doom <- investigatorSpace iid >>= maybe (pure 0) (fmap (.doom) . getSpace)
        pure
          [ Reaction
              "jenica-capra"
              "Jenica Capra: remove one doom from your space"
              [ResolveEffect (EffectCtx iid (SourceCard cid) Nothing) (RemoveDoomFrom YourSpace (N 1))]
          | doom > 0
          ]
      _ -> pure []

{- | She answers damage any investigator dealt, so long as the monster is in her
own space. Her damage comes from the card, so it does not answer itself.
-}
litaChantler :: AssetBehavior
litaChantler =
  defaultAssetBehavior
    & #afterMonsterDamaged
    .~ \cid owner mid src -> case src of
      SourceInvestigator _ -> do
        mine <- investigatorSpace owner
        theirs <- uses #monsters (fmap (.space) . Map.lookup mid)
        pure [DealMonsterDamage mid (SourceCard cid) 1 | isJust mine, mine == theirs]
      _ -> pure []

-- | Two successes for the cat itself, while an evade action's test resolves.
strayCat :: AssetBehavior
strayCat =
  defaultAssetBehavior
    & #testOptions
    .~ \cid _ ts ->
      pure
        [ Reaction
            "stray-cat"
            "Stray Cat: discard to add two successes"
            [DiscardAsset cid, AddTestSuccesses 2, ContinueTest]
        | ActionTest EvadeAction _ <- [ts.kind]
        ]
