-- | Mechanics for the cards in the item deck.
module AH3e.Content.ItemBehaviors (behaviors) where

import AH3e.Engine.Behavior
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
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
      , ("tattered-cloak", defaultAssetBehavior & #ignoredByMonsters .~ True)
      , ("token-of-faith", tokenOfFaith)
      , ("shotgun", shotgun)
      ]

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
