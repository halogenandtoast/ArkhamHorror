-- | Mechanics for the special pile: the named cards encounters hand out.
module AH3e.Content.SpecialBehaviors (behaviors) where

import AH3e.Engine.Behavior
import AH3e.Engine.Helpers
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
  ( mempty
      & #customEffects
      .~ Map.fromList
        [ ("clover-club-gamble", gamble)
        , ("mysterious-serum", serum)
        , ("abandoned-luggage-stash", stash)
        ]
      & #customAfterTests
      .~ Map.fromList [("abandoned-luggage", openLuggage)]
  )
    & #assets
    .~ Map.fromList
      [
        ( "ace-of-rods"
        , rerollInstead "ace-of-rods" "Ace of Rods: reroll any number of dice instead" liveDiceCount
        )
      , ("abandoned-luggage", abandonedLuggage)
      , ("astrolabe", astrolabe)
      , ("clover-club-member", cloverClubMember)
      , ("contraband-whiskey", contrabandWhiskey)
      , ("dark-blessing", darkBlessing)
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
      ,
        ( "mysterious-serum"
        , cardAction "Mysterious Serum: discard to recover fully" (Custom "mysterious-serum")
        )
      , ("performer", gatherTalent "performer" "Performer" "merchant-district" Influence)
      , ("rare-books-access", rareBooksAccess)
      , ("reporting-gig", reportingGig)
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

{- | "After you perform a gather resources action in the Downtown neighborhood, you
may spend $2 to gamble."
-}
cloverClubMember :: AssetBehavior
cloverClubMember =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterGatherResources iid -> do
        here <- investigatorNeighborhood iid
        i <- getInvestigator iid
        let ctx = EffectCtx iid (SourceCard cid) Nothing
        pure
          [ Reaction
              "clover-club-member"
              "Clover Club Member: spend $2 to gamble"
              [ResolveEffect ctx (Pay (SpendMoney 2) (Custom "clover-club-gamble"))]
          | here == Just "downtown"
          , i.money >= 2
          ]
      _ -> pure []

-- | rule 474: a roll outside a test, so nothing can reroll or modify it
gamble :: EffectCtx -> GameM ()
gamble ctx = do
  n <- rollDie
  logText ("Rolled " <> tshow n <> " and gains $" <> tshow n)
  addMoney ctx.investigator n

{- | "Action: Discard this card to recover all of your health and sanity and focus
one skill of your choice, even if it exceeds your focus limit."
-}
serum :: EffectCtx -> GameM ()
serum ctx = do
  i <- getInvestigator ctx.investigator
  pushAll
    $ [DiscardAsset cid | SourceCard cid <- [ctx.source]]
    <> [ RecoverInvestigator ctx.investigator i.damage i.horror
       , ResolveEffect ctx (Focus Nothing True)
       ]

{- | "After you have an encounter in the Miskatonic University neighborhood, you may
discard one tome if you have one. If you do, or if you have no tomes, you gain one
tome item."
-}
rareBooksAccess :: AssetBehavior
rareBooksAccess =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterEncounter iid -> do
        here <- investigatorNeighborhood iid
        let tome = GainE (AnItem (Just "Tome"))
            swap = If (HasCard (WithTrait "Tome")) (Pay (CostDiscard (WithTrait "Tome")) tome) tome
        pure
          [ Reaction
              "rare-books-access"
              "Rare Books Access: trade a tome for a tome"
              [ResolveEffect (EffectCtx iid (SourceCard cid) Nothing) swap]
          | here == Just "miskatonic-university"
          ]
      _ -> pure []

{- | "+1 observation as part of a research action. After you gain a clue, you gain
\$2." The money is not offered but taken, once per handful of clues.
-}
reportingGig :: AssetBehavior
reportingGig =
  testBonuses [OnAction ResearchAction Observation 1]
    & #afterGainClue
    .~ \cid iid -> pure [ResolveEffect (EffectCtx iid (SourceCard cid) Nothing) (GainE (Money (N 2)))]

{- | "While resolving a test, 4s, 5s, and 6s count as successes. Roll two dice
while resolving the reckoning effect of your DARK PACT. You cannot be BLESSED or
CURSED." The pact's own reckoning reads this card by name.
-}
darkBlessing :: AssetBehavior
darkBlessing =
  defaultAssetBehavior
    & #successOnFour
    .~ True
    & #bansConditions
    .~ ["BLESSED", "CURSED"]

{- | "When you gain this card from the deck, place the top two cards of the item
deck facedown under this card. Action: Test observation -1. If you pass, you gain
those items and discard this card."
-}
abandonedLuggage :: AssetBehavior
abandonedLuggage =
  defaultAssetBehavior
    & #afterGainedFromDeck
    ?~ Custom "abandoned-luggage-stash"
    & #componentActions
    .~ [ ComponentActionDef
           { label = "Abandoned Luggage: test observation to open it"
           , allowedWhileEngaged = False
           , canPerform = \_ -> pure True
           , perform = \ctx -> for_ [cid | SourceCard cid <- [ctx.source]] \cid ->
               push
                 $ BeginTest
                   ( newTest
                       ctx.investigator
                       Observation
                       (-1)
                       OtherTest
                       (AfterCustom (SourceCard cid) "abandoned-luggage")
                   )
           }
       ]

{- | The two items wait as assets of their own, attached to the luggage and left out
of their owner's cards, so they cannot be used until the luggage is opened.
Discarding the luggage takes them with it.
-}
stash :: EffectCtx -> GameM ()
stash ctx = for_ [cid | SourceCard cid <- [ctx.source]] \cid -> do
  deck <- use (#decks . #item)
  let (taken, rest) = splitAt 2 deck
  #decks . #item .= rest
  for_ taken \item -> do
    removeCardEverywhere item
    #assets . at item ?= Asset item ctx.investigator 0 0 True (Just cid) mempty
  logText (tshow (length taken) <> " items are tucked under the luggage")

-- | Passing the test hands over what was under the luggage and the luggage goes.
openLuggage :: Source -> Int -> GameM ()
openLuggage src r = for_ [cid | SourceCard cid <- [src]] \cid -> do
  a <- use (#assets . at cid)
  for_ a \luggage -> when (r > 0) do
    under <- uses #assets (filter ((== Just cid) . (.attachedTo)) . Map.elems)
    for_ under \x -> do
      #assets . ix x.card . #attachedTo .= Nothing
      #assets . ix x.card . #flipped .= False
      investigatorL luggage.owner . #assets %= (<> [x.card])
    push (DiscardAsset cid)
