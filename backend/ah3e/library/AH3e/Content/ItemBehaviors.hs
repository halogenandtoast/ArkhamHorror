-- | Mechanics for the cards in the item deck.
module AH3e.Content.ItemBehaviors (behaviors) where

import AH3e.Engine.Behavior
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Effect
import AH3e.Types.Skill
import AH3e.Types.State
import Data.Map.Strict qualified as Map

behaviors :: Behaviors
behaviors =
  mempty
    & #assets
    .~ Map.fromList
      [ ("38-revolver", testBonuses [OnAction AttackAction Strength 2])
      , ("41-derringer", derringer)
      , ("bulletproof-vest", defaultAssetBehavior & #preventsOwnHarm ?~ (DamageStat, 2, 1))
      , ("45-automatic", testBonuses [OnAction AttackAction Strength 3])
      , ("45-thompson", testBonuses [OnAction AttackAction Strength 5])
      ,
        ( "fine-clothes"
        , defaultAssetBehavior & #halfPricePerRound .~ True
        )
      ,
        ( "first-aid-kit"
        , cardAction "First Aid Kit: recover one health" (RecoverHealth InvestigatorOrAllyInYourSpace (N 1))
        )
      ,
        ( "grimms-fairy-tales"
        , cardAction
            "Grimms' Fairy Tales: recover one sanity"
            (RecoverSanity InvestigatorOrAllyInYourSpace (N 1))
        )
      , ("dynamite", dynamite)
      , ("elder-sign-amulet", defaultAssetBehavior & #preventsOwnHarm ?~ (HorrorStat, 2, 1))
      , ("grotesque-statue", grotesqueStatue)
      , ("knife", testBonuses [OnAction AttackAction Strength 1])
      , ("lucky-cigarette-case", luckyCigaretteCase)
      , ("leather-coat", testBonuses [OnAction EvadeAction Observation 1])
      , ("magnifying-glass", testBonuses [OnAction ResearchAction Observation 1])
      , ("mystic-scroll", testBonuses [WhileCasting 2])
      , ("mystic-tome", testBonuses [WhileCasting 3])
      , ("occult-scripture", testBonuses [OnAction ResearchAction Observation 2])
      , ("otherworld-codex", testBonuses [OnAction WardAction Lore 3])
      , ("pocket-watch", defaultAssetBehavior & #extraActions .~ 1)
      , ("rabbits-foot", defaultAssetBehavior & #freeRerollPerRound .~ True)
      , ("secret-page", testBonuses [OnAction WardAction Lore 2])
      , ("silver-key", silverKey)
      , ("tattered-cloak", defaultAssetBehavior & #ignoredByMonsters .~ \_ _ -> pure True)
      , ("token-of-faith", tokenOfFaith)
      , ("shotgun", shotgun)
      , -- Dead of Night
        ("camera", camera)
      , ("map-of-arkham", mapOfArkham)
      , ("true-magick", trueMagick)
      , ("warding-stone", wardingStone)
      ]
    & #customEffects
    .~ Map.fromList [("camera-research", cameraResearch)]

{- | Once per attack test -- the card prints no round limit, and an attack action
is one opportunity to use it. It adds no dice, so it reports a bonus of none at
all just to be offered as a test asset: it is a one handed weapon, and taking it
up has to cost that hand.
-}
derringer :: AssetBehavior
derringer =
  defaultAssetBehavior
    & #testDice
    .~ (\_ _ ts -> pure (if isAttackTest ts then Just 0 else Nothing))
    & #testOptions
    .~ \cid _ ts ->
      pure
        [ Reaction
            "41-derringer"
            "41 Derringer: add one to a die"
            [MarkUsedInTest cid, AddToDie (SourceCard cid)]
        | isAttackTest ts
        , cid `elem` ts.chosenAssets
        , cid `notElem` ts.usedInTest
        , liveDiceCount ts > 0
        ]

isAttackTest :: TestState -> Bool
isAttackTest ts = case ts.kind of
  ActionTest AttackAction _ -> True
  _ -> False

luckyCigaretteCase :: AssetBehavior
luckyCigaretteCase =
  defaultAssetBehavior
    & #testOptions
    .~ \cid iid ts -> do
      used <- usedThisRound cid iid
      pure
        [ Reaction
            "lucky-cigarette-case"
            "Lucky Cigarette Case: add one to a die"
            [MarkAssetUsed iid cid, AddToDie (SourceCard cid)]
        | not used
        , liveDiceCount ts > 0
        ]

silverKey :: AssetBehavior
silverKey =
  defaultAssetBehavior
    & #testOptions
    .~ \cid iid ts -> do
      used <- usedThisRound cid iid
      let live = liveDiceCount ts
      pure
        [ Reaction
            "silver-key"
            "Silver Key: reroll any number of dice"
            [MarkAssetUsed iid cid, RerollUpTo (SourceCard cid) live]
        | not used
        , live > 0
        , ts.skill `elem` [Lore, Observation]
        ]

{- | "After this item suffers one or more horror, you recover one sanity" -- it
answers even when that horror was its third and destroyed it.
-}
tokenOfFaith :: AssetBehavior
tokenOfFaith =
  defaultAssetBehavior
    & #afterHarm
    .~ \cid iid plan ->
      pure [RecoverInvestigator iid 0 1 | Just (self, k) <- [plan.horrorTo], self == cid, k > 0]

{- | "After you perform a research action, you may suffer one horror to research
one clue." Researching moves a clue of your own, so it needs one to move.
-}
grotesqueStatue :: AssetBehavior
grotesqueStatue =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterResearchAction iid -> do
        i <- getInvestigator iid
        pure
          [ Reaction
              "grotesque-statue"
              "Grotesque Statue: suffer one horror to research one clue"
              [SufferHarm iid (SourceCard cid) NormalHarm 0 1, ResearchCluesExact iid 1]
          | i.clues > 0
          ]
      _ -> pure []

{- | Five damage to everything engaged with you, for the card itself. Offered while
an attack action's test resolves, which is the "as part of" the card prints.
-}
dynamite :: AssetBehavior
dynamite =
  defaultAssetBehavior
    & #testOptions
    .~ \cid iid ts -> do
      engaged <- engagedMonsters iid
      pure
        [ Reaction
            "dynamite"
            "Dynamite: discard to deal five damage to each monster engaged with you"
            ( DiscardAsset cid
                : [DealMonsterDamage m.card (SourceCard cid) 5 | m <- engaged]
                  <> [ContinueTest]
            )
        | isAttackTest ts
        , not (null engaged)
        ]

{- | +5 strength, and each six counts twice. The six already counted once as a
success, so each one adds one more.
-}
shotgun :: AssetBehavior
shotgun =
  testBonuses [OnAction AttackAction Strength 5]
    & #extraSuccesses
    .~ \cid _ ts ->
      pure
        $ if isAttackTest ts && cid `elem` ts.chosenAssets
          then length [d | d <- ts.dice, not d.removed, d.value >= 6]
          else 0

{- | "After you gain a clue, you may test observation -1. If you pass, research
one clue." The test is the card's, not an action, so it rides the effect
vocabulary's own test rather than an action test.
-}
camera :: AssetBehavior
camera =
  defaultAssetBehavior
    & #afterGainClue
    .~ \cid iid ->
      pure
        [ ResolveEffect
            (EffectCtx iid (SourceCard cid) Nothing)
            ( May
                "Camera: test observation -1 to research one clue"
                (Test Observation (-1) (Custom "camera-research") NoEffect)
            )
        ]

-- | Researching moves a clue of your own, so it needs one to move.
cameraResearch :: EffectCtx -> GameM ()
cameraResearch ctx = do
  i <- getInvestigator ctx.investigator
  when (i.clues > 0) $ push (ResearchCluesExact ctx.investigator 1)

{- | "While you are in a street space, monsters do not engage you unless you
attack them." The hook reads this alongside its own provocation check, which is
the "unless you attack them" half, so this only answers for the space.
-}
mapOfArkham :: AssetBehavior
mapOfArkham =
  defaultAssetBehavior
    & #ignoredByMonsters
    .~ \_ iid ->
      investigatorSpace iid >>= \case
        Nothing -> pure False
        Just sid -> isStreetLike . (.kind) <$> getSpace sid

{- | "+3 lore while casting a spell. Each 6 you roll while casting a spell counts
as two successes." The six already counted once as a success, so each one adds
one more; like the Shotgun, it only speaks for a test it was taken up for.
-}
trueMagick :: AssetBehavior
trueMagick =
  testBonuses [WhileCasting 3]
    & #extraSuccesses
    .~ \cid _ ts ->
      pure
        $ if isCastingTest ts && cid `elem` ts.chosenAssets
          then length [d | d <- ts.dice, not d.removed, d.value >= 6]
          else 0

isCastingTest :: TestState -> Bool
isCastingTest ts = isJust ts.casting || isSpellTest ts.kind

{- | "Once per round, as part of a ward action, you may spend one remnant to
reroll any number of dice." Nothing else checks the reaction's cost, so the
remnant has to be in hand before it is offered at all.
-}
wardingStone :: AssetBehavior
wardingStone =
  defaultAssetBehavior
    & #testOptions
    .~ \cid iid ts -> do
      used <- usedThisRound cid iid
      affordable <- canPayCost iid (SpendRemnants 1)
      let live = liveDiceCount ts
      pure
        [ Reaction
            "warding-stone"
            "Warding Stone: spend one remnant to reroll any number of dice"
            [ MarkAssetUsed iid cid
            , PayCost (EffectCtx iid (SourceCard cid) Nothing) (SpendRemnants 1)
            , RerollUpTo (SourceCard cid) live
            ]
        | not used
        , affordable
        , live > 0
        , ActionTest WardAction _ <- [ts.kind]
        ]
