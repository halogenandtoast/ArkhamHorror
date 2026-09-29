-- | What the Dead of Night investigators' own cards do.
module AH3e.Content.DeadOfNight.StartingBehaviors (behaviors) where

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
import Data.Text qualified as T
import Text.Read (readMaybe)

behaviors :: Behaviors
behaviors =
  mempty
    { assets =
        Map.fromList
          [ ("38-special", thirtyEightSpecial)
          , ("switchblade", switchblade)
          , ("dark-insight", darkInsight)
          , ("call-the-storm", callTheStorm)
          , ("replicable-findings", replicableFindings)
          , ("follow-up", insteadOfRemnant' "Follow Up" "spawn a clue instead" [SpawnClue])
          , ("implacable", implacable)
          , ("research-notes", defaultAssetBehavior & #raiseInsteadOfReroll .~ True)
          , ("light-fingers", lightFingers)
          , ("on-the-lam", onTheLam)
          , ("stolen-amulet", stolenAmulet)
          , ("flux-stabilizer", fluxStabilizer)
          ]
    , customEffects =
        Map.fromList
          [ ("call-the-storm", storm)
          , ("light-fingers", lightFingersPick)
          , ("on-the-lam", laylow)
          ]
    , customAfterTests =
        Map.fromList [("call-the-storm", stormResult), ("light-fingers", lightFingersResult)]
    }

{- | ".38 Special: you get +2 strength as part of an attack action. If the monster
you are attacking has a remnant icon, you get +3 strength instead."
-}
thirtyEightSpecial :: AssetBehavior
thirtyEightSpecial =
  defaultAssetBehavior
    & #testDice
    .~ \_ _ ts -> case ts.kind of
      ActionTest AttackAction mtarget | ts.skill == Strength -> do
        rich <- maybe (pure False) (fmap (.remnant) . monsterDef) mtarget
        pure (Just (if rich then 3 else 2))
      _ -> pure Nothing

-- | "Switchblade: when you disengage a monster, you may deal one damage to it."
switchblade :: AssetBehavior
switchblade =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterDisengage _ mid -> do
        there <- uses #monsters (Map.member mid)
        pure
          [ Reaction "switchblade" "Switchblade: deal one damage" [DealMonsterDamage mid (SourceCard cid) 1]
          | there
          ]
      _ -> pure []

{- | "Dark Insight: once per round, while resolving a test, you may add one to the
result of a number of dice equal to the amount of doom in your space."
-}
darkInsight :: AssetBehavior
darkInsight =
  defaultAssetBehavior
    & #testOptions
    .~ \cid iid ts -> do
      used <- usedThisRound cid iid
      doom <- maybe (pure 0) (fmap (.doom) . getSpace) =<< investigatorSpace iid
      pure
        [ Reaction
            "dark-insight"
            ("Dark Insight: add one to " <> tshow doom <> " dice")
            (MarkAssetUsed iid cid : replicate doom (AddToDie (SourceCard cid)))
        | not used && doom > 0 && liveDiceCount ts > 0
        ]

{- | "Call the Storm. Action: choose any space and test lore -1. Each monster in that
space suffers damage equal to your test result. Then, place one doom in that space."
-}
callTheStorm :: AssetBehavior
callTheStorm =
  defaultAssetBehavior
    & #componentActions
    .~ [ ComponentActionDef
           { label = "Call the Storm"
           , allowedWhileEngaged = False
           , canPerform = \_ -> pure True
           , perform = \ctx -> push (ResolveEffect ctx (Custom "call-the-storm"))
           }
       ]

{- | The space is chosen before the spell is cast, so the card remembers where the
storm was aimed while the test resolves.
-}
storm :: EffectCtx -> GameM ()
storm ctx = case ctx.source of
  SourceCard cid -> do
    spaces <- uses (#board . #spaces) Map.keys
    chooseFor
      ctx.investigator
      "Choose a space for the storm"
      [Choice (SpaceLabel sid) [RememberOnCard cid (coerce sid), castStorm ctx cid] | sid <- spaces]
  _ -> pure ()

castStorm :: EffectCtx -> CardId -> Message
castStorm ctx cid =
  CastSpell
    ctx.investigator
    cid
    [ BeginTest
        (newTest ctx.investigator Lore (-1) (SpellTest cid) (AfterCustom (SourceCard cid) "call-the-storm"))
          { casting = Just cid
          }
    ]

-- | Every monster in the space the card was aimed at, and then the doom it leaves.
stormResult :: Source -> Int -> GameM ()
stormResult src r = case src of
  SourceCard cid -> do
    aimed <- uses #assets (maybe [] (Map.keys . (.tokens)) . Map.lookup cid)
    assetL cid . #tokens .= mempty
    for_ (take 1 aimed) \key -> do
      let sid = SpaceId key
      ms <- uses #monsters (filter ((== sid) . (.space)) . Map.elems)
      pushAll
        $ [DealMonsterDamage m.card src r | r > 0, m <- ms]
        <> [PlaceDoom src sid]
  _ -> pure ()

{- | "Replicable Findings: when you place two or more clues on the scenario sheet as
part of a single research action, you may place one additional clue from the clue
pool."
-}
replicableFindings :: AssetBehavior
replicableFindings =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterCluesResearched iid k ->
        pure
          [ Reaction
              "replicable-findings"
              "Replicable Findings: place one more clue on the sheet"
              [ResolveEffect (EffectCtx iid (SourceCard cid) Nothing) (PlaceCluesOnSheet (N 1))]
          | k >= 2
          ]
      _ -> pure []

-- | "When you would gain a remnant, you may <do this> instead."
insteadOfRemnant' :: Text -> Text -> [Message] -> AssetBehavior
insteadOfRemnant' name what messages =
  defaultAssetBehavior
    & #insteadOfRemnant
    .~ \_ _ -> pure [Reaction (T.toLower name) (name <> ": " <> what) messages]

-- | "Implacable: ... you may recover one health and one sanity instead."
implacable :: AssetBehavior
implacable =
  defaultAssetBehavior
    & #insteadOfRemnant
    .~ \_ iid ->
      pure
        [ Reaction
            "implacable"
            "Implacable: recover one health and one sanity instead"
            [RecoverInvestigator iid 1 1]
        ]

{- | "Light Fingers: after you perform a gather resources action, you may become
WANTED to choose one item in the display and test observation +1. If your test
result equals or exceeds that item's value, gain that item and discard WANTED."
-}
lightFingers :: AssetBehavior
lightFingers =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterGatherResources iid ->
        pure
          [ Reaction
              "light-fingers"
              "Light Fingers: become WANTED to lift something from the display"
              [ GainConditionMsg iid "WANTED"
              , ResolveEffect (EffectCtx iid (SourceCard cid) Nothing) (Custom "light-fingers")
              ]
          ]
      _ -> pure []

-- | The item is chosen before the test, so the card remembers which one.
lightFingersPick :: EffectCtx -> GameM ()
lightFingersPick ctx = case ctx.source of
  SourceCard cid -> do
    display <- use (#decks . #display)
    unless (null display)
      $ chooseFor
        ctx.investigator
        "Choose an item to lift"
        [ Choice
            (CardLabel c)
            [ RememberOnCard cid (tshow (coerce c :: Int))
            , BeginTest
                (newTest ctx.investigator Observation 1 OtherTest (AfterCustom (SourceCard cid) "light-fingers"))
            ]
        | c <- display
        ]
  _ -> pure ()

-- | Its value is the bar: clear it and the item is yours, and WANTED goes away.
lightFingersResult :: Source -> Int -> GameM ()
lightFingersResult src r = case src of
  SourceCard cid -> do
    picked <- uses #assets (maybe [] (Map.keys . (.tokens)) . Map.lookup cid)
    assetL cid . #tokens .= mempty
    owner <- uses #assets (fmap (.owner) . Map.lookup cid)
    for_ ((,) <$> owner <*> (listToMaybe picked >>= readMaybe . T.unpack)) \(iid, n) -> do
      let target = CardId n
      value <- maybe 0 (fromMaybe 0 . (.value)) <$> assetDef target
      if r >= value
        then do
          logText "Light Fingers: it walks"
          wanted <- conditionCard iid "WANTED"
          pushAll ([GainFromDisplay iid target] <> map DiscardAsset (maybeToList wanted))
        else logText "Light Fingers: your hand is not quick enough"
  _ -> pure ()

{- | "On the Lam: during your turn, you may suffer one horror to disengage and
exhaust all non-epic monsters engaged with you; non-epic monsters do not engage
you until the end of the round."
-}
onTheLam :: AssetBehavior
onTheLam =
  defaultAssetBehavior
    & #freeActions
    .~ [ ComponentActionDef
           { label = "On the Lam: suffer one horror to slip away"
           , allowedWhileEngaged = True
           , canPerform = \iid -> not <$> usedAbility iid hiddenForTheRound
           , perform = \ctx -> push (ResolveEffect ctx (Pay (CostHorror 1) (Custom "on-the-lam")))
           }
       ]

laylow :: EffectCtx -> GameM ()
laylow ctx = do
  let iid = ctx.investigator
  ms <- uses #monsters (filter (isEngagedWith iid) . Map.elems)
  ordinary <- filterM (fmap (not . (.epic)) . monsterDef . (.card)) ms
  logText "You slip away into the night"
  pushAll
    $ [MarkAbilityUsed iid hiddenForTheRound]
    <> concat [[DisengageMonster iid m.card, ExhaustMonster m.card] | m <- ordinary]

{- | "Stolen Amulet: once per round, during your turn, you may suffer one direct
horror to perform one additional action."
-}
stolenAmulet :: AssetBehavior
stolenAmulet =
  defaultAssetBehavior
    & #freeActions
    .~ [ ComponentActionDef
           { label = "Stolen Amulet: suffer one direct horror for another action"
           , allowedWhileEngaged = True
           , canPerform = \iid -> not <$> usedAbility iid "stolen-amulet"
           , perform = \ctx -> do
               investigatorL ctx.investigator . #bonusActions += 1
               pushAll
                 [ MarkAbilityUsed ctx.investigator "stolen-amulet"
                 , SufferHarm ctx.investigator ctx.source DirectHarm 0 1
                 ]
           }
       ]

{- | "Flux Stabilizer: once per round, when a doom or non-epic monster would be
placed in your neighborhood, you may discard it instead." The monster goes back to
its deck rather than being defeated, since nothing here defeats it.
-}
fluxStabilizer :: AssetBehavior
fluxStabilizer =
  defaultAssetBehavior
    & #stopsPlacement
    .~ \cid iid what _ ->
      pure
        [ Reaction
            "flux-stabilizer"
            ( "Flux Stabilizer: discard the "
                <> (case what of PlacingDoom -> "doom"; PlacingMonster _ -> "monster")
                <> " instead"
            )
            ( MarkAssetUsed iid cid
                : case what of
                  PlacingDoom -> []
                  PlacingMonster mid -> [DiscardMonster mid]
            )
        ]
