-- | Mechanics for the cards in the item deck.
module AH3e.Content.ItemBehaviors (behaviors) where

import AH3e.Engine.Behavior
import AH3e.Engine.Query
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
      , ("45-automatic", testBonuses [OnAction AttackAction Strength 3])
      , ("45-thompson", testBonuses [OnAction AttackAction Strength 5])
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
      , -- the second half, each 6 counting as two successes, has no seam yet
        ("shotgun", testBonuses [OnAction AttackAction Strength 5])
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
    & #dieOptions
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
    & #dieOptions
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
    & #dieOptions
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
