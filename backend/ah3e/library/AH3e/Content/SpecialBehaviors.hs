-- | Mechanics for the special pile: the named cards encounters hand out.
module AH3e.Content.SpecialBehaviors (behaviors) where

import AH3e.Engine.Behavior
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
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
        ( "ace-of-rods"
        , rerollInstead "ace-of-rods" "Ace of Rods: reroll any number of dice instead" liveDiceCount
        )
      , ("astrolabe", astrolabe)
      , ("contraband-whiskey", contrabandWhiskey)
      , ("deputy-of-arkham", deputyOfArkham)
      , ("gravedigger", gatherTalent "gravedigger" "Gravedigger" "rivertown" Strength)
      ,
        ( "innsmouth-look"
        , rerollInstead
            "innsmouth-look"
            "Innsmouth Look: reroll dice up to the horror you have suffered"
            (const 0)
            & #reckoning
            ?~ MayPay (SpendFocus 1) NoEffect (SufferHorror (N 1))
        )
      , ("performer", gatherTalent "performer" "Performer" "merchant-district" Influence)
      , ("server-at-velmas", gatherTalent "server-at-velmas" "Server at Velma's" "easttown" Influence)
      , ("service-piece", testBonuses [OnAction AttackAction Strength 2])
      , ("stevedore", gatherTalent "stevedore" "Stevedore" "merchant-district" Strength)
      , ("wooden-homunculus", woodenHomunculus)
      ]

{- | "After you perform a gather resources action in <neighborhood>, test <skill>.
If you pass, you gain an additional $2."
-}
gatherTalent :: Text -> Text -> NeighborhoodId -> Skill -> AssetBehavior
gatherTalent key name nid skill =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterGatherResources iid -> do
        here <- investigatorNeighborhood iid
        let ctx = EffectCtx iid (SourceCard cid) Nothing
            earn = Test skill 0 (GainE (Money (N 2))) NoEffect
        pure
          [Reaction key (name <> ": test for an additional $2") [ResolveEffect ctx earn] | here == Just nid]
      _ -> pure []

{- | "After you perform a gather resources action in the Merchant District or
Downtown neighborhoods, you may place one horror on this card to gain $2."
-}
contrabandWhiskey :: AssetBehavior
contrabandWhiskey =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterGatherResources iid -> do
        here <- investigatorNeighborhood iid
        pure
          [ Reaction
              "contraband-whiskey"
              "Contraband Whiskey: place one horror on it to gain $2"
              [HarmAsset cid 0 1, ResolveEffect (EffectCtx iid (SourceCard cid) Nothing) (GainE (Money (N 2)))]
          | here `elem` map Just ["merchant-district", "downtown"]
          ]
      _ -> pure []

{- | A card that widens the reroll a focus paid for: the focus bought one die, and
this offers the rest. @howMany@ counts them from the test, zero meaning the pool.
-}
rerollInstead :: Text -> Text -> (TestState -> Int) -> AssetBehavior
rerollInstead key lbl howMany =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      SpentFocusToReroll iid -> do
        mts <- use #test
        i <- getInvestigator iid
        used <- usedThisRound cid iid
        pure case mts of
          Just ts | not used -> do
            let n = if howMany ts == 0 then i.horror else howMany ts
            [ Reaction key lbl [MarkAssetUsed iid cid, RerollUpTo (SourceCard cid) (min (liveDiceCount ts) n)]
              | n > 0
              , liveDiceCount ts > 0
              ]
          _ -> []
      _ -> pure []

{- | "As part of a ward action, roll one additional die for each clue you have and
one additional die for each clue in your neighborhood." Not once a round, so it
rides on 'testDice' rather than on the once-a-round dice.
-}
astrolabe :: AssetBehavior
astrolabe =
  defaultAssetBehavior
    & #testDice
    .~ \_ iid ts -> case ts.kind of
      ActionTest WardAction _ -> do
        i <- getInvestigator iid
        here <- neighborhoodClues iid
        pure $ if i.clues + here > 0 then Just (i.clues + here) else Nothing
      _ -> pure Nothing

-- | "Once per round, as part of an attack action, you may reroll one or all of your dice."
woodenHomunculus :: AssetBehavior
woodenHomunculus =
  defaultAssetBehavior
    & #testOptions
    .~ \cid iid ts -> do
      used <- usedThisRound cid iid
      let live = liveDiceCount ts
          offer k lbl ms = Reaction k ("Wooden Homunculus: " <> lbl) (MarkAssetUsed iid cid : ms)
      pure
        [ o
        | not used
        , live > 0
        , ActionTest AttackAction _ <- [ts.kind]
        , o <-
            [ offer "wooden-homunculus-one" "reroll one die" [RerollUpTo (SourceCard cid) 1]
            , offer "wooden-homunculus-all" "reroll all dice" [RerollAll (SourceCard cid)]
            ]
        ]

{- | "+1 observation during the encounter phase." The other half, its focus limit,
is data on the card itself (see 'AH3e.Content.Special.focusLimitBonuses').
-}
deputyOfArkham :: AssetBehavior
deputyOfArkham =
  defaultAssetBehavior
    & #testDice
    .~ \_ _ ts -> do
      phase <- use #phase
      pure $ if phase == EncounterPhase && ts.skill == Observation then Just 1 else Nothing
