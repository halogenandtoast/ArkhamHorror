-- | Mechanics for the special pile: the named cards encounters hand out.
module AH3e.Content.SpecialBehaviors (behaviors) where

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

behaviors :: Behaviors
behaviors =
  ( mempty
      & #customEffects
      .~ Map.fromList
        [ ("clover-club-gamble", gamble)
        , ("mysterious-serum", serum)
        , ("abandoned-luggage-stash", stash)
        , ("puzzle-box", openPuzzleBox)
        , ("friend-of-a-friend-discard", friendOfAFriendDiscard)
        , ("friend-of-a-friend-gain", friendOfAFriendGain)
        , ("lonnie-ritter-spend", lonnieRitterSpend)
        , ("lonnie-ritter-repair", lonnieRitterRepair)
        , ("strangers-contract", strangersContractClears)
        , ("cryptic-sketches", crypticSketchesFocus)
        , ("guiding-spirit", guidingSpiritTurn)
        , ("inner-sanctum-access-take", innerSanctumTake)
        , ("inner-sanctum-access-place", innerSanctumPlace)
        , ("the-red-clock", theRedClockEscape)
        , ("unknown-liturgy", unknownLiturgyCast)
        , ("michael-leigh-move", michaelLeighMove)
        ]
      & #customAfterTests
      .~ Map.fromList
        [ ("abandoned-luggage", openLuggage)
        , ("good-standing", goodStandingPrice)
        , ("unknown-liturgy", unknownLiturgyResult)
        , ("michael-leigh", michaelLeighResult)
        ]
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
      , ("bocce-champion", bocceChampion)
      , ("library-docent", libraryDocent)
      , ("valued-donor", valuedDonor)
      , ("joey-vigils-supply", joeyVigilsSupply)
      , ("wooden-homunculus", woodenHomunculus)
      , ("armed-backup", armedBackup)
      , ("black-grimoire", blackGrimoire)
      ,
        ( "cleaner"
        , defeatBounty
            "Cleaner: recover one sanity or focus one skill"
            "Sheldon"
            [("Recover one sanity", RecoverSanity You (N 1)), ("Focus one skill", Focus Nothing False)]
        )
      ,
        ( "donohues-new-45s"
        , testBonuses [OnAction AttackAction Strength 3] & #freeRerollPerRound .~ True
        )
      , ("friendly-raven", friendlyRaven)
      , ("good-standing", goodStanding)
      , ("grave-dirt", graveDirt)
      , ("hired-muscle", hiredMuscle)
      , ("hypnotists-mirror", hypnotistsMirror)
      , -- "As part of a trade action, you may also exchange focus tokens and talents."
        ("mi-go-brain-case", defaultAssetBehavior & #tradesFocusAndTalents .~ True)
      ,
        ( "legbreaker"
        , defeatBounty
            "Legbreaker: recover one health or gain $1"
            "O'Bannion"
            [("Recover one health", RecoverHealth You (N 1)), ("Gain $1", GainE (Money (N 1)))]
        )
      , ("leo-de-luca", leoDeLuca)
      ,
        ( "maeve-chapman"
        , cardAction "Maeve Chapman: recover one health" (RecoverHealth InvestigatorOrAllyInYourSpace (N 1))
        )
      ,
        ( "mas-apple-pie"
        , homeCooking
            "mas-apple-pie"
            "Ma's Apple Pie: put one horror on it to recover one sanity"
            (0, 1)
            (RecoverSanity InvestigatorOrAllyInYourSpace (N 1))
        )
      , -- the other half of Miles Crown, his focus limit, is data on the card
        ("miles-crown", defaultAssetBehavior & #focusPerSkill .~ 2)
      , -- "While performing a test, you can use one additional hand's worth of assets."
        ("peter-sylvestre", defaultAssetBehavior & #handsDelta .~ 1)
      , ("puzzle-box", puzzleBox)
      ,
        ( "schoffners-catalogue"
        , cardAction
            "Schoffner's Catalogue: buy one common item for $1 more"
            (BuyFromDisplay (Just "Common") (Markup 1) (Just 1) NoEffect)
        )
      , ("smuggler-contacts", smugglerContacts)
      , ("the-star", theStar)
      , ("the-world", theWorld)
      , ("trusted-source", trustedSource)
      ,
        ( "velmas-cherry-pie"
        , homeCooking
            "velmas-cherry-pie"
            "Velma's Cherry Pie: put one damage on it to recover one health"
            (1, 0)
            (RecoverHealth InvestigatorOrAllyInYourSpace (N 1))
        )
      , ("witchweed", witchweed)
      , -- Under Dark Waves
        ("death", death)
      , ("eben-halls-journal", ebenHallsJournal)
      , ("eye-for-appraisal", eyeForAppraisal)
      , ("four-of-cups", fourOfCups)
      , ("friend-of-a-friend", friendOfAFriend)
      ,
        ( "golden-crown"
        , testBonuses [WhileCasting 2]
            & #reckoning
            ?~ Choose
              [ ("Place one doom in your space", PlaceDoomAt YourSpace (N 1))
              , ("Suffer one horror", SufferHorror (N 1))
              ]
        )
      , ("harpoon", harpoon)
      , ("hotel-porter", hotelPorter)
      , ("inuksuk", inuksuk)
      , ("kerosene", kerosene)
      , ("lonnie-ritter", lonnieRitter)
      , ("lucky-coin", luckyCoin)
      , ("strangers-contract", strangersContract)
      , ("twisted-flesh", twistedFlesh)
      , -- Secrets of the Order
        ("aquinnah", aquinnah)
      , ("blackened-athame", blackenedAthame)
      , ("book-of-shadows", bookOfShadows)
      , ("cabbies-favor", defaultAssetBehavior & #extraStepWhenPaying .~ 1)
      , ("chthonian-stone", chthonianStone)
      , ("cryptic-sketches", crypticSketches)
      , ("cyclopean-hammer", cyclopeanHammer)
      , ("david-renfield", davidRenfield)
      , ("guiding-spirit", guidingSpirit)
      ,
        ( "hidden-routes"
        , cardAction
            "Hidden Routes: discard a focus to slip into any street"
            (Pay (SpendFocus 1) (MoveDirectlyTo AnyStreetSpace))
        )
      , ("inner-sanctum-access", innerSanctumAccess)
      , ("lost-journal", lostJournal)
      , ("michael-leigh", michaelLeigh)
      , ("nine-of-rods", nineOfRods)
      , ("steward-of-the-order", stewardOfTheOrder)
      , ("the-hierophant", theHierophant)
      , ("the-red-clock", theRedClock)
      , ("unknown-liturgy", unknownLiturgy)
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

{- | "After you perform a gather resources action in the Downtown neighborhood, you
may test observation. If you pass, gain an additional $3. If you fail, discard
this card."
-}
bocceChampion :: AssetBehavior
bocceChampion =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterGatherResources iid -> do
        here <- investigatorNeighborhood iid
        let ctx = EffectCtx iid (SourceCard cid) Nothing
            earn = Test Observation 0 (GainE (Money (N 3))) (Custom "discard-source")
        pure
          [ Reaction "bocce-champion" "Bocce Champion: test for an additional $3" [ResolveEffect ctx earn]
          | here == Just "downtown"
          ]
      _ -> pure []

{- | "After you perform a gather resources action in the Miskatonic University
neighborhood, each investigator in your neighborhood may focus lore."
-}
libraryDocent :: AssetBehavior
libraryDocent =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterGatherResources iid -> do
        here <- investigatorNeighborhood iid
        let ctx = EffectCtx iid (SourceCard cid) Nothing
            teach = ForInvestigators InSourceNeighborhood (Focus (Just Lore) False)
        pure
          [ Reaction
              "library-docent"
              "Library Docent: your neighborhood may focus lore"
              [ResolveEffect ctx teach]
          | here == Just "miskatonic-university"
          ]
      _ -> pure []

{- | "After you perform a gather resources action in the Southside neighborhood, you
may spend one remnant to focus one skill of your choice."
-}
valuedDonor :: AssetBehavior
valuedDonor =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterGatherResources iid -> do
        here <- investigatorNeighborhood iid
        let ctx = EffectCtx iid (SourceCard cid) Nothing
        pure
          [ Reaction
              "valued-donor"
              "Valued Donor: spend a remnant to focus a skill"
              [ResolveEffect ctx (Pay (SpendRemnants 1) (Focus Nothing False))]
          | here == Just "southside"
          ]
      _ -> pure []

{- | "Increase the size of the display by one card. If JOEY VIGIL'S SUPPLY is
discarded, discard the item in the display with the highest value."
-}
joeyVigilsSupply :: AssetBehavior
joeyVigilsSupply =
  defaultAssetBehavior
    & #displayDelta
    .~ (\_ -> pure 1)
    & #onDiscard
    .~ \cid iid -> pure [ResolveEffect (EffectCtx iid (SourceCard cid) Nothing) (Custom "discard-richest-item")]

{- | "Action: Test lore -1. If you pass, gain one spell. If you fail, suffer one
horror or place one doom in your space."
-}
blackGrimoire :: AssetBehavior
blackGrimoire =
  cardAction "Black Grimoire: test lore for a spell" (Test Lore (-1) (GainE (ASpell Nothing)) toll)
 where
  toll =
    Choose
      [ ("Suffer one horror", SufferHorror (N 1))
      , ("Place one doom in your space", PlaceDoomAt YourSpace (N 1))
      ]

{- | "Once per round, during your turn, you may spend $1 to deal one damage to a
monster in your space." Kept back when there is no monster, since the dollar is
spent before one is chosen.
-}
hiredMuscle :: AssetBehavior
hiredMuscle =
  defaultAssetBehavior
    & #freeActions
    .~ [ ComponentActionDef
           { label = "Hired Muscle: spend $1 to deal one damage"
           , allowedWhileEngaged = True
           , canPerform = \iid -> do
               used <- usedAbility iid "hired-muscle"
               i <- getInvestigator iid
               here <- maybe (pure []) monstersAt i.space
               pure (not used && i.money >= 1 && not (null here))
           , perform = \ctx -> do
               spendOncePerRound ctx.investigator "hired-muscle"
               push (ResolveEffect ctx (Pay (SpendMoney 1) (DamageMonsterIn YourSpace (N 1))))
           }
       ]

{- | "Once per round, as an additional action during your turn, you may perform an
action that you have already performed this round." A granted action costs them
none of their own, and its repeat flag is what skips the used-up check.
-}
leoDeLuca :: AssetBehavior
leoDeLuca =
  defaultAssetBehavior
    & #freeActions
    .~ [ ComponentActionDef
           { label = "Leo De Luca: repeat an action"
           , allowedWhileEngaged = True
           , canPerform = \iid -> do
               used <- usedAbility iid "leo-de-luca"
               done <- (.performed) <$> getInvestigator iid
               pure (not used && not (null done))
           , perform = \ctx -> do
               let iid = ctx.investigator
               done <- (.performed) <$> getInvestigator iid
               spendOncePerRound iid "leo-de-luca"
               chooseFor
                 iid
                 "Repeat an action"
                 [Choice (ActionLabel k) [PerformGrantedAction iid k True] | k <- done]
           }
       ]

{- | "After you perform a gather resources action, you may deal one <harm> to this
item for an investigator or ally in your space to recover one <harm>." The harm is
dealt to the card whatever it holds, so it is only offered when someone can use
what it buys.
-}
homeCooking :: Text -> Text -> (Int, Int) -> Effect -> AssetBehavior
homeCooking key lbl (dmg, hor) recovery =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterGatherResources iid -> do
        let ctx = EffectCtx iid (SourceCard cid) Nothing
        useful <- effectUseful ctx recovery
        pure [Reaction key lbl [HarmAsset cid dmg hor, ResolveEffect ctx recovery] | useful]
      _ -> pure []

{- | "Action: Test lore -1. If you pass, gain two curios from the deck (not the
display) and discard this card."
-}
puzzleBox :: AssetBehavior
puzzleBox =
  cardAction "Puzzle Box: test lore to open it" (Test Lore (-1) (Custom "puzzle-box") NoEffect)

{- | The curios come off the item deck, which 'GainE' cannot ask for: it would
offer the display as well.
-}
openPuzzleBox :: EffectCtx -> GameM ()
openPuzzleBox ctx =
  pushAll
    $ replicate 2 (GainItemFromDeck ctx.investigator ItemDeckKind (Just "Curio") Nothing)
    <> [DiscardAsset cid | SourceCard cid <- [ctx.source]]

{- | "Action: Spend any number of remnants to gain $1 for each remnant spent this
way." The first remnant is paid outright, which keeps the action off the menu of
anyone who has none; the rest are asked for one at a time.
-}
smugglerContacts :: AssetBehavior
smugglerContacts =
  cardAction
    "Smuggler Contacts: spend remnants for $1 each"
    (Pay (SpendRemnants 1) (Seq [earn, RepeatWhilePaying (SpendRemnants 1) earn]))
 where
  earn = GainE (Money (N 1))

{- | "Once per round, while resolving a test, if there are one or more clues in your
neighborhood, you may reroll one die."
-}
trustedSource :: AssetBehavior
trustedSource =
  defaultAssetBehavior
    & #testOptions
    .~ \cid iid ts -> do
      used <- usedThisRound cid iid
      clues <- neighborhoodClues iid
      pure
        [ Reaction
            "trusted-source"
            "Trusted Source: reroll one die"
            [MarkAssetUsed iid cid, RerollUpTo (SourceCard cid) 1]
        | not used
        , clues > 0
        , liveDiceCount ts > 0
        ]

{- | "When this item suffers one or more horror, you may focus one skill of your
choice, even if it exceeds your focus limit." It answers even when that horror was
its third and discarded it.
-}
witchweed :: AssetBehavior
witchweed =
  defaultAssetBehavior
    & #afterHarm
    .~ \cid iid plan ->
      pure
        [ ResolveEffect
            (EffectCtx iid (SourceCard cid) Nothing)
            (May "Witchweed: focus one skill" (Focus Nothing True))
        | Just (self, k) <- [plan.horrorTo]
        , self == cid
        , k > 0
        ]

{- | One offer per monster that could take the exhaust, all under one key: which
offer is taken is how the monster is chosen, and the shared key keeps the window's
re-check from coming back around with the rest once one has been used. The cost
rides on the offer, so nothing is paid without a monster to spend it on. What
cannot be exhausted at all is not offered, since 'ExhaustMonster' would quietly do
nothing with the cost already paid (and a ready shrouded monster would be named
besides).
-}
exhaustOffers :: Text -> Text -> [Message] -> [Monster] -> GameM [Reaction]
exhaustOffers key lbl cost ms =
  concat <$> for ms \m -> do
    d <- monsterDef m.card
    name <- (.name) <$> getCardDef m.card
    ok <- canBeExhausted m.card
    pure
      [ Reaction key (lbl <> name) (cost <> [ExhaustMonster m.card])
      | not d.epic
      , ok
      ]

{- | "Once per round, at the end of the monster phase, you may spend $1 to exhaust
one non-epic monster in your space." The monster phase ends once a round, so the
trigger is the limit; the dollar is only offered while there is one to spend.
-}
armedBackup :: AssetBehavior
armedBackup =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AtEndOfMonsterPhase iid -> do
        i <- getInvestigator iid
        here <- maybe (pure []) monstersAt i.space
        let ctx = EffectCtx iid (SourceCard cid) Nothing
        exhaustOffers
          "armed-backup"
          "Armed Backup: spend $1 to exhaust "
          [PayCost ctx (SpendMoney 1)]
          (if i.money >= 1 then here else [])
      _ -> pure []

{- | "At the start of your turn, you may deal one damage to this ally to exhaust one
non-epic monster in your neighborhood." The damage is dealt alongside the exhaust
rather than ahead of an ask the raven may not survive to be asked, and a street
holds no neighborhood, so it offers nothing there.
-}
friendlyRaven :: AssetBehavior
friendlyRaven =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AtStartOfTurn iid -> do
        mnid <- investigatorNeighborhood iid
        spaces <- maybe (pure []) (uses #board . neighborhoodSpaces) mnid
        ms <- concat <$> traverse monstersAt spaces
        exhaustOffers "friendly-raven" "Friendly Raven: damage it to exhaust " [HarmAsset cid 1 0] ms
      _ -> pure []

{- | "When you defeat a <trait> monster, you may <one of these>." Only its holder
finishing the monster counts, whatever the rest of the table does, and the monster
is still on the board here, so its traits read off its own card.
-}
defeatBounty :: Text -> Trait -> [(Text, Effect)] -> AssetBehavior
defeatBounty lbl trait options =
  defaultAssetBehavior
    & #afterMonsterDefeated
    .~ \cid owner mid src -> do
      d <- monsterDef mid
      pure
        [ ResolveEffect (EffectCtx owner (SourceCard cid) Nothing) (May lbl (Choose options))
        | SourceInvestigator who <- [src]
        , who == owner
        , trait `elem` d.traits
        ]

{- | "After you perform a move action, if you moved more than two spaces, you may
focus one skill of your choice." The count is the spaces the action carried them,
which the move action zeroes as it starts.
-}
theWorld :: AssetBehavior
theWorld =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterMoveDistance iid moved ->
        pure
          [ Reaction
              "the-world"
              "The World: focus one skill"
              [ResolveEffect (EffectCtx iid (SourceCard cid) Nothing) (Focus Nothing False)]
          | moved > 2
          ]
      _ -> pure []

{- | "Once per round, when buying an item, you may test influence. Reduce the value
of that item by your test result, to a minimum of $1." The reduction is not known
until the test is, so the card it was tested for and that card's price are noted on
the talent, and the purchase itself waits for the result. Nothing is offered for a
card already down to a dollar, which no result could better.
-}
goodStanding :: AssetBehavior
goodStanding =
  defaultAssetBehavior
    & #buyOffers
    .~ \cid iid target price -> do
      used <- usedThisRound cid iid
      name <- (.name) <$> getCardDef target
      pure
        [ Reaction
            "good-standing"
            ("Good Standing: test influence to lower " <> name <> "'s price")
            [ MarkAssetUsed iid cid
            , NoteOnCard cid "buy-card" (coerce target)
            , NoteOnCard cid "buy-price" price
            , BeginTest (newTest iid Influence 0 OtherTest (AfterCustom (SourceCard cid) "good-standing"))
            ]
        | not used
        , price > 1
        ]

-- | The purchase Good Standing tested for, at whatever the test brought it down to.
goodStandingPrice :: Source -> Int -> GameM ()
goodStandingPrice src r = for_ [cid | SourceCard cid <- [src]] \cid -> do
  ma <- use (#assets . at cid)
  for_ ma \a -> do
    let noted what = Map.lookup what a.tokens
    case (noted "buy-card", noted "buy-price") of
      (Just target, Just price) -> do
        assetL cid . #tokens .= mempty
        i <- getInvestigator a.owner
        display <- use (#decks . #display)
        let reduced = max 1 (price - r)
            card = coerce target
        when (card `elem` display && i.money >= reduced) $ push (BuyCard a.owner card reduced)
      _ -> pure ()

{- | "While resolving a test, if you are not CURSED, you may suffer one direct
horror to change one die to a 6. After resolving the test, become CURSED." The
curse comes only once the test is over, so the dirt can be used more than once
during it; gaining a condition already held does nothing, so the riders do not
need weeding.
-}
graveDirt :: AssetBehavior
graveDirt =
  defaultAssetBehavior
    & #testOptions
    .~ \cid iid ts -> do
      cursed <- hasCondition iid "CURSED"
      let ctx = EffectCtx iid (SourceCard cid) Nothing
      pure
        [ Reaction
            "grave-dirt"
            "Grave Dirt: suffer one direct horror to change one die to a 6"
            [ ResolveEffect ctx (DirectHorror (N 1))
            , -- the rider is left behind before the die is chosen, since that choice
              -- can carry the test all the way to its end
              AddTestRider ctx (GainE (Condition "CURSED"))
            , ChooseDieToSet 6
            ]
        | not cursed
        , liveDiceCount ts > 0
        ]

{- | "Once per round, when an ally or investigator in your space recovers sanity,
they recover one additional sanity." The trigger reaches everyone standing where
the recovery happened, so the mirror's holder is asked wherever it was theirs.
-}
hypnotistsMirror :: AssetBehavior
hypnotistsMirror =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterRecoverSanity iid target -> do
        used <- usedThisRound cid iid
        ally <- case target of
          RecoveredInvestigator _ -> pure True
          RecoveredAsset c -> maybe False ((== Ally) . (.assetType)) <$> assetDef c
        let more = case target of
              RecoveredInvestigator who -> RecoverInvestigator who 0 1
              RecoveredAsset c -> RecoverAsset c 0 1
        pure
          [ Reaction
              "hypnotists-mirror"
              "Hypnotist's Mirror: recover one additional sanity"
              [MarkAssetUsed iid cid, more]
          | not used
          , ally
          ]
      _ -> pure []

{- | "When one or more mythos tokens are added or returned to the mythos cup, you
may recover one health or one sanity." Offered only where there is something to
recover, since the choice is between two.
-}
theStar :: AssetBehavior
theStar =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      TokensReturnedToCup iid _ -> do
        i <- getInvestigator iid
        let ctx = EffectCtx iid (SourceCard cid) Nothing
            options =
              [("Recover one health", RecoverHealth You (N 1)) | i.damage > 0]
                <> [("Recover one sanity", RecoverSanity You (N 1)) | i.horror > 0]
        pure
          [ Reaction
              "the-star"
              "The Star: recover one health or one sanity"
              [ResolveEffect ctx (Choose options)]
          | not (null options)
          ]
      _ -> pure []

{- | "After a card is added to the codex or a card in the codex is flipped, you may
remove one doom from any space or spawn one clue." The codex changes often enough
that the offer is kept to what could do something: with no doom anywhere on the
board, only the clue is worth naming.
-}
death :: AssetBehavior
death =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterCodexChanged iid -> do
        spaces <- traverse getSpace =<< allNeighborhoodSpaces
        let options =
              [ ("Remove one doom from any space", RemoveDoomFrom AnySpaceWithDoom (N 1))
              | any ((> 0) . (.doom)) spaces
              ]
                <> [("Spawn one clue", SpawnOneClue)]
        pure
          [ Reaction
              "death"
              "Death: remove one doom from any space or spawn one clue"
              [ResolveEffect (EffectCtx iid (SourceCard cid) Nothing) (Choose options)]
          ]
      _ -> pure []

{- | "+2 lore while casting a spell. After you perform a gather resources action in
Kingsport, you may test lore -1. If you pass, you gain one spell." Kingsport is a
town rather than a neighborhood, so either of its tiles answers.
-}
ebenHallsJournal :: AssetBehavior
ebenHallsJournal =
  testBonuses [WhileCasting 2]
    & #reactions
    .~ \cid -> \case
      AfterGatherResources iid -> do
        town <- investigatorTown iid
        let ctx = EffectCtx iid (SourceCard cid) Nothing
            study = Test Lore (-1) (GainE (ASpell Nothing)) NoEffect
        pure
          [ Reaction "eben-halls-journal" "Eben Hall's Journal: test lore for a spell" [ResolveEffect ctx study]
          | town == Just Kingsport
          ]
      _ -> pure []

{- | "Before you would buy or gain one or more curios, you may discard and replace
one item from the display." Cycling one card is that swap, and it is offered while
the shelf can still be read by whatever is about to take from it.
-}
eyeForAppraisal :: AssetBehavior
eyeForAppraisal =
  defaultAssetBehavior
    & #reactions
    .~ \_ -> \case
      BeforeAcquiring iid mtrait
        | mtrait == Just "Curio" ->
            pure
              [ Reaction
                  "eye-for-appraisal"
                  "Eye for Appraisal: discard and replace one item from the display"
                  [CycleDisplay iid 1]
              ]
      _ -> pure []

{- | "Once per round, while performing a test, if you are the only investigator in
your neighborhood, you may reroll any number of dice."
-}
fourOfCups :: AssetBehavior
fourOfCups =
  defaultAssetBehavior
    & #testOptions
    .~ \cid iid ts -> do
      used <- usedThisRound cid iid
      alone <- onlyInvestigatorInNeighborhood iid
      let live = liveDiceCount ts
      pure
        [ Reaction
            "four-of-cups"
            "Four of Cups: reroll any number of dice"
            [MarkAssetUsed iid cid, RerollUpTo (SourceCard cid) live]
        | not used
        , alone
        , live > 0
        ]

{- | "After you resolve a street encounter, you may discard an item to gain one item
of equal or lesser value from the display." An item with no printed value sets no
price, so it cannot be the one traded in.
-}
friendOfAFriend :: AssetBehavior
friendOfAFriend =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterStreetEncounter iid -> do
        tradable <- pricedItems iid
        pure
          [ Reaction
              "friend-of-a-friend"
              "Friend of a Friend: trade an item in for one of equal or lesser value"
              [ResolveEffect (EffectCtx iid (SourceCard cid) Nothing) (Custom "friend-of-a-friend-discard")]
          | not (null tradable)
          ]
      _ -> pure []

-- | The items an investigator holds that carry a printed value, with it.
pricedItems :: InvestigatorId -> GameM [(CardId, Int)]
pricedItems iid = do
  items <- matchingAssets iid ItemCard
  catMaybes <$> for items \cid -> fmap (cid,) <$> cardValue cid

{- | The item traded in sets what the replacement may cost, so its value is noted on
the talent and the display is read once it has gone. A discarded item goes to the
bottom of the item deck rather than onto the shelf, so it cannot be bought back.
-}
friendOfAFriendDiscard :: EffectCtx -> GameM ()
friendOfAFriendDiscard ctx = for_ [c | SourceCard c <- [ctx.source]] \self -> do
  tradable <- pricedItems ctx.investigator
  chooseFor
    ctx.investigator
    "Discard an item"
    [ Choice
        (CardLabel cid)
        [ NoteOnCard self "swap-value" v
        , DiscardAsset cid
        , ResolveEffect ctx (Custom "friend-of-a-friend-gain")
        ]
    | (cid, v) <- tradable
    ]

-- | The replacement comes off the display alone, at the value noted a moment ago.
friendOfAFriendGain :: EffectCtx -> GameM ()
friendOfAFriendGain ctx = for_ [c | SourceCard c <- [ctx.source]] \self -> do
  noted <- uses #assets (Map.lookup "swap-value" . maybe mempty (.tokens) . Map.lookup self)
  for_ noted \v -> do
    assetL self . #tokens .= mempty
    display <- use (#decks . #display)
    eligible <- filterM (itemMatches Nothing (Just (AtMost v))) display
    chooseFor
      ctx.investigator
      ("Gain an item worth $" <> tshow v <> " or less")
      [Choice (CardLabel cid) [GainFromDisplay ctx.investigator cid] | cid <- eligible]

{- | "+3 strength as part of an attack action. Before you perform an attack action,
you may move a monster in an adjacent space to your space." The haul happens before
the target is chosen, so what it drags in can be what is attacked. It is not once a
round, but every offer shares one key, so only one monster comes in per action.
-}
harpoon :: AssetBehavior
harpoon =
  testBonuses [OnAction AttackAction Strength 3]
    & #reactions
    .~ \_ -> \case
      BeforePerformAction iid AttackAction -> do
        msid <- investigatorSpace iid
        board <- use #board
        ms <- concat <$> traverse monstersAt (maybe [] (`adjacentSpaces` board) msid)
        for [(m, sid) | m <- ms, sid <- maybeToList msid] \(m, sid) -> do
          name <- (.name) <$> getCardDef m.card
          pure
            $ Reaction "harpoon" ("Harpoon: haul " <> name <> " into your space") [MoveMonsterTo m.card sid]
      _ -> pure []

{- | "After you perform a gather resources action in the Innsmouth Shore
neighborhood, you gain an additional $1 for each focus you have." Nothing is
offered to someone holding no focus, since the dollar count would be zero.
-}
hotelPorter :: AssetBehavior
hotelPorter =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterGatherResources iid -> do
        here <- investigatorNeighborhood iid
        n <- focusCount <$> getInvestigator iid
        pure
          [ Reaction
              "hotel-porter"
              ("Hotel Porter: gain an additional $" <> tshow n)
              [ResolveEffect (EffectCtx iid (SourceCard cid) Nothing) (GainE (Money (N n)))]
          | here == Just "innsmouth-shore"
          , n > 0
          ]
      _ -> pure []

{- | "Encounter: Move an unengaged investigator from any space to any space in
another neighborhood." Taking it is the encounter, so it is offered in the
encounter phase in place of the card that would be read.
-}
inuksuk :: AssetBehavior
inuksuk =
  defaultAssetBehavior
    & #encounterAbilities
    .~ [ ComponentActionDef
           { label = "Inuksuk: move an unengaged investigator to another neighborhood"
           , allowedWhileEngaged = False
           , canPerform = \_ -> not . null <$> unengagedInvestigators
           , perform = \ctx -> do
               travellers <- map (.id) <$> unengagedInvestigators
               push (ChooseInvestigatorsFor ctx 1 travellers (MoveDirectlyTo SpaceInAnotherNeighborhood))
           }
       ]

{- | "When you would gain a remnant, you may instead discard this card to remove one
doom from your space and for you or an ally to recover two sanity."
-}
kerosene :: AssetBehavior
kerosene =
  defaultAssetBehavior
    & #insteadOfRemnant
    .~ \cid iid ->
      pure
        [ Reaction
            "kerosene"
            "Kerosene: burn it to remove one doom from your space and recover two sanity"
            [ DiscardAsset cid
            , ResolveEffect
                (EffectCtx iid (SourceCard cid) Nothing)
                (Seq [RemoveDoomFrom YourSpace (N 1), RecoverSanity YouOrAlly (N 2)])
            ]
        ]

{- | "Action: Spend up to $3 and choose an investigator in your space. One of that
investigator's items recovers health equal to the amount spent." Kept off the menu
while nothing in the space is damaged or there is no dollar to spend.
-}
lonnieRitter :: AssetBehavior
lonnieRitter =
  defaultAssetBehavior
    & #componentActions
    .~ [ ComponentActionDef
           { label = "Lonnie Ritter: spend up to $3 to mend an item"
           , allowedWhileEngaged = False
           , canPerform = \iid -> do
               i <- getInvestigator iid
               damaged <- damagedItemsInSpace iid
               pure (i.money >= 1 && not (null damaged))
           , perform = \ctx -> push (ResolveEffect ctx (Custom "lonnie-ritter-spend"))
           }
       ]

-- | The damaged items held by anyone standing in this investigator's space.
damagedItemsInSpace :: InvestigatorId -> GameM [CardId]
damagedItemsInSpace iid = do
  sid <- investigatorSpace iid
  here <- maybe (pure []) investigatorsAt sid
  items <- filterM (cardMatches ItemCard) (concatMap (.assets) here)
  filterM (\c -> uses #assets (maybe False ((> 0) . (.damage)) . Map.lookup c)) items

{- | The money is spent before the item is chosen, so only amounts that could mend
something are offered and the one chosen is noted for the second half.
-}
lonnieRitterSpend :: EffectCtx -> GameM ()
lonnieRitterSpend ctx = for_ [c | SourceCard c <- [ctx.source]] \self -> do
  let iid = ctx.investigator
  i <- getInvestigator iid
  damaged <- damagedItemsInSpace iid
  worst <- maximum . (0 :) <$> for damaged \c -> uses #assets (maybe 0 (.damage) . Map.lookup c)
  chooseFor
    iid
    "Spend up to $3 to mend an item"
    [ Choice
        (AmountLabel n)
        [ PayCost ctx (SpendMoney n)
        , NoteOnCard self "mend" n
        , ResolveEffect ctx (Custom "lonnie-ritter-repair")
        ]
    | n <- [1 .. minimum [3, i.money, worst]]
    ]

-- | The item mended, for the money already spent on it.
lonnieRitterRepair :: EffectCtx -> GameM ()
lonnieRitterRepair ctx = for_ [c | SourceCard c <- [ctx.source]] \self -> do
  noted <- uses #assets (Map.lookup "mend" . maybe mempty (.tokens) . Map.lookup self)
  for_ noted \n -> do
    assetL self . #tokens .= mempty
    damaged <- damagedItemsInSpace ctx.investigator
    chooseFor
      ctx.investigator
      ("Choose an item to recover " <> tshow n <> " health")
      [Choice (CardLabel cid) [RecoverAsset cid n 0] | cid <- damaged]

{- | "After you roll a die, you may discard this card to change that die roll to a
result of your choice." Rolls outside a test are resolved where they are made
(rule 474), so the coin answers the dice on the table, which is every roll the
engine keeps long enough to change.
-}
luckyCoin :: AssetBehavior
luckyCoin =
  defaultAssetBehavior
    & #testOptions
    .~ \cid _ ts ->
      pure
        [ Reaction
            "lucky-coin"
            "Lucky Coin: spend it to change a die to a result of your choice"
            [DiscardAsset cid, ChooseDieResult]
        | liveDiceCount ts > 0
        ]

{- | "During your turn, you may discard this card and gain a DARK PACT to defeat all
non-epic monsters in your space and remove all doom from your space." The pact is
the price, so it is not asked for while the space holds nothing to clear.
-}
strangersContract :: AssetBehavior
strangersContract =
  defaultAssetBehavior
    & #freeActions
    .~ [ ComponentActionDef
           { label = "Stranger's Contract: sign it to clear your space"
           , allowedWhileEngaged = True
           , canPerform = spaceWorthClearing
           , perform = \ctx ->
               pushAll
                 $ [DiscardAsset cid | SourceCard cid <- [ctx.source]]
                 <> [ ResolveEffect ctx (GainE (Condition "DARK PACT"))
                    , ResolveEffect ctx (Custom "strangers-contract")
                    ]
           }
       ]

-- | Whether the contract would clear anything: a non-epic monster, or any doom.
spaceWorthClearing :: InvestigatorId -> GameM Bool
spaceWorthClearing iid =
  investigatorSpace iid >>= \case
    Nothing -> pure False
    Just sid -> do
      nonEpic <- nonEpicMonstersAt sid
      doom <- (.doom) <$> getSpace sid
      pure (doom > 0 || not (null nonEpic))

nonEpicMonstersAt :: SpaceId -> GameM [Monster]
nonEpicMonstersAt sid = monstersAt sid >>= filterM (fmap (not . (.epic)) . monsterDef . (.card))

{- | The monsters go together rather than one at a time, since the contract names
them all at once, and the space's doom goes with them.
-}
strangersContractClears :: EffectCtx -> GameM ()
strangersContractClears ctx = do
  msid <- investigatorSpace ctx.investigator
  for_ msid \sid -> do
    nonEpic <- nonEpicMonstersAt sid
    doom <- (.doom) <$> getSpace sid
    pushAll
      $ [DefeatMonster m.card ctx.source | m <- nonEpic]
      <> [RemoveDoom sid doom | doom > 0]

{- | "When this talent is discarded, draw and resolve two tokens from the mythos
cup." Its three health are what usually discards it.
-}
twistedFlesh :: AssetBehavior
twistedFlesh =
  defaultAssetBehavior
    & #onDiscard
    .~ \cid iid -> pure [ResolveEffect (EffectCtx iid (SourceCard cid) Nothing) (DrawMythosTokens 2)]

-- Secrets of the Order --------------------------------------------------------

cardCtx :: InvestigatorId -> CardId -> EffectCtx
cardCtx iid cid = EffectCtx iid (SourceCard cid) Nothing

-- | How much of that pile a card is keeping on itself.
cardNote :: CardId -> Text -> GameM Int
cardNote cid key = uses #assets (Map.findWithDefault 0 key . maybe mempty (.tokens) . Map.lookup cid)

-- | The one space a card noted on itself, under @space:@, and nothing else.
notedSpace :: CardId -> GameM (Maybe SpaceId)
notedSpace cid = do
  keys <- uses #assets (Map.keys . maybe mempty (.tokens) . Map.lookup cid)
  pure (listToMaybe (mapMaybe (fmap SpaceId . T.stripPrefix "space:") keys))

noteSpace :: CardId -> SpaceId -> Message
noteSpace cid sid = NoteOnCard cid ("space:" <> coerce sid) 1

{- | "When a monster attacks you, you may deal one horror to this ally to prevent all
damage and horror dealt to you by that attack and deal one damage to a monster in
your space." Printed without a round limit, so nothing marks her used; she is only
offered while she has the sanity to pay.
-}
aquinnah :: AssetBehavior
aquinnah =
  defaultAssetBehavior
    & #damagePrevention
    .~ \cid owner plan -> do
      d <- assetDef cid
      a <- use (assetL cid)
      let attacked = case plan.source of SourceMonster _ -> True; _ -> False
      pure
        [ Reaction
            "aquinnah"
            "Aquinnah: deal her one horror to turn the attack aside"
            [ HarmAsset cid 0 1
            , PreventedHarm plan.damage plan.horror
            , ResolveEffect (cardCtx owner cid) (DamageMonsterIn YourSpace (N 1))
            ]
        | plan.investigator == owner
        , attacked
        , plan.damage + plan.horror > 0
        , a.horror < fromMaybe 0 (d >>= (.sanity))
        ]

{- | "While casting a spell, you may suffer one damage to reroll any number of dice."
A one-handed card, so it has to be taken up for the test to be used in it, and it
adds no dice of its own.
-}
blackenedAthame :: AssetBehavior
blackenedAthame =
  defaultAssetBehavior
    & #testDice
    .~ (\_ _ ts -> pure (if isCastingTest ts then Just 0 else Nothing))
    & #testOptions
    .~ \cid iid ts -> do
      let live = liveDiceCount ts
      pure
        [ Reaction
            "blackened-athame"
            "Blackened Athame: suffer one damage to reroll any number of dice"
            [ SufferHarm iid (SourceCard cid) NormalHarm 1 0
            , RerollUpTo (SourceCard cid) live
            ]
        | isCastingTest ts
        , cid `elem` ts.chosenAssets
        , live > 0
        ]

{- | "Once per round, when you would suffer horror to cast a spell, you may prevent
that horror. If you fail the test to cast that spell, suffer that spell's horror
twice."

The cast's horror is paid before its test exists, so the doubled horror is left as
a rider for the next test to begin, which is that cast's own.
-}
bookOfShadows :: AssetBehavior
bookOfShadows =
  defaultAssetBehavior
    & #damagePrevention
    .~ \cid owner plan -> do
      cost <- case plan.source of
        SourceCard spell -> do
          d <- assetDef spell
          pure $ case d of
            Just a | a.assetType == Spell -> a.spellHorror
            _ -> 0
        _ -> pure 0
      pure
        [ Reaction
            "book-of-shadows"
            "Book of Shadows: prevent the horror this spell costs"
            [ MarkAssetUsed owner cid
            , PreventedHarm 0 plan.horror
            , AddTestRider
                (cardCtx owner cid)
                (ByResult [((0, Just 0), SufferHorror (N (2 * cost)))])
            ]
        | plan.investigator == owner
        , plan.damage == 0
        , plan.horror > 0
        , cost > 0
        ]

{- | "Once per round, while resolving a test, you may reroll one die or all dice. If
you do, place one doom in your space after that test." One or all, so the two are
offered as they are printed rather than as any number; the doom rides on the test.
-}
chthonianStone :: AssetBehavior
chthonianStone =
  defaultAssetBehavior
    & #testOptions
    .~ \cid iid ts -> do
      used <- usedThisRound cid iid
      let live = liveDiceCount ts
          offer key lbl ms =
            Reaction
              key
              ("Chthonian Stone: " <> lbl)
              ( [ MarkAssetUsed iid cid
                , AddTestRider (cardCtx iid cid) (PlaceDoomAt YourSpace (N 1))
                ]
                  <> ms
              )
      pure
        [ o
        | not used
        , live > 0
        , o <-
            [ offer "chthonian-stone-one" "reroll one die" [RerollUpTo (SourceCard cid) 1]
            , offer "chthonian-stone-all" "reroll all dice" [RerollAll (SourceCard cid)]
            ]
        ]

{- | "After a non-human monster spawns, you may discard one remnant to focus one
skill of your choice." Every investigator hears about a spawn, wherever it landed.
-}
crypticSketches :: AssetBehavior
crypticSketches =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterMonsterSpawned iid mid -> do
        d <- monsterDef mid
        i <- getInvestigator iid
        -- the remnant is gone before the skill is chosen, so nothing is offered
        -- to someone with no skill left to focus
        room <- effectUseful (cardCtx iid cid) (Focus Nothing False)
        pure
          [ Reaction
              "cryptic-sketches"
              "Cryptic Sketches: discard a remnant to focus one skill"
              [ResolveEffect (cardCtx iid cid) (Custom "cryptic-sketches")]
          | "Human" `notElem` d.traits
          , i.remnants > 0
          , room
          ]
      _ -> pure []

-- | The remnant is discarded rather than spent, so nothing answers it going.
crypticSketchesFocus :: EffectCtx -> GameM ()
crypticSketchesFocus ctx = do
  addRemnants ctx.investigator (-1)
  push (ResolveEffect ctx (Focus Nothing False))

{- | "+4 strength as part of an attack action. You may always test strength while
performing an attack action." The hammer answers whatever skill the monster prints,
except strength, which needs no offer.
-}
cyclopeanHammer :: AssetBehavior
cyclopeanHammer =
  testBonuses [OnAction AttackAction Strength 4]
    & #attackSkillInstead
    .~ \printed -> Strength <$ guard (printed /= Strength)

{- | "At the end of your turn, you may place one doom in your space for this ally to
recover two horror."
-}
davidRenfield :: AssetBehavior
davidRenfield =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AtEndOfTurn iid -> do
        a <- use (assetL cid)
        pure
          [ Reaction
              "david-renfield"
              "David Renfield: place one doom in your space for him to recover two horror"
              [ResolveEffect (cardCtx iid cid) (PlaceDoomAt YourSpace (N 1)), RecoverAsset cid 0 2]
          | a.owner == iid
          , a.horror > 0
          ]
      _ -> pure []

{- | "Once per round, you may discard the top card of your neighborhood's encounter
deck. If you discard an event this way, discard one clue from your neighborhood and
spawn two clues." Printed with no window of its own, so it is taken during its
owner's turn and costs them no action.
-}
guidingSpirit :: AssetBehavior
guidingSpirit =
  defaultAssetBehavior
    & #freeActions
    .~ [ ComponentActionDef
           { label = "Guiding Spirit: turn over the top of your encounter deck"
           , allowedWhileEngaged = True
           , canPerform = \iid -> do
               used <- usedAbility iid "guiding-spirit"
               mnid <- investigatorNeighborhood iid
               deck <- use (encounterDeckLens mnid)
               pure (not used && not (null deck))
           , perform = \ctx -> do
               spendOncePerRound ctx.investigator "guiding-spirit"
               push (ResolveEffect ctx (Custom "guiding-spirit"))
           }
       ]

{- | A discarded event goes to the event discard, which is where an event whose clue
has been taken goes; anything else goes under its own deck, there being no other
pile for an encounter card.
-}
guidingSpiritTurn :: EffectCtx -> GameM ()
guidingSpiritTurn ctx = do
  mnid <- investigatorNeighborhood ctx.investigator
  discardTop mnid (encounterDeckLens mnid)
 where
  discardTop :: Maybe NeighborhoodId -> Lens' Game [CardId] -> GameM ()
  discardTop mnid l =
    use l >>= \case
      [] -> logText "The spirit has nothing left to show"
      (cid : rest) -> do
        d <- getCardDef cid
        logText ("Guiding Spirit discards " <> d.name)
        case d.kind of
          EventCard _ -> do
            l .= rest
            #decks . #eventDiscard %= (cid :)
            for_ mnid \nid -> neighborhoodL nid . #clues %= max 0 . subtract 1
            pushAll [SpawnClue, SpawnClue]
          _ -> l .= rest <> [cid]

{- | "After you perform a gather resources action in the French Hill neighborhood,
you may remove all doom from any space and place an equal amount of doom in any
other space." The amount is not known until the space is chosen, so it and the
space it came from are noted on the talent and read back by the second half.
-}
innerSanctumAccess :: AssetBehavior
innerSanctumAccess =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterGatherResources iid -> do
        here <- investigatorNeighborhood iid
        spaces <- traverse getSpace =<< allNeighborhoodSpaces
        pure
          [ Reaction
              "inner-sanctum-access"
              "Inner Sanctum Access: move all the doom from one space to another"
              [ResolveEffect (cardCtx iid cid) (Custom "inner-sanctum-access-take")]
          | here == Just "french-hill"
          , any ((> 0) . (.doom)) spaces
          ]
      _ -> pure []

innerSanctumTake :: EffectCtx -> GameM ()
innerSanctumTake ctx = for_ [c | SourceCard c <- [ctx.source]] \self -> do
  spaces <- filter ((> 0) . (.doom)) <$> (traverse getSpace =<< allNeighborhoodSpaces)
  chooseFor ctx.investigator "Remove all the doom from a space"
    $ [ Choice
          (SpaceLabel s.id)
          [ RemoveDoom s.id s.doom
          , NoteOnCard self "doom" s.doom
          , noteSpace self s.id
          , ResolveEffect ctx (Custom "inner-sanctum-access-place")
          ]
      | s <- spaces
      ]

innerSanctumPlace :: EffectCtx -> GameM ()
innerSanctumPlace ctx = for_ [c | SourceCard c <- [ctx.source]] \self -> do
  n <- cardNote self "doom"
  from <- notedSpace self
  assetL self . #tokens .= mempty
  spaces <- filter (\sid -> Just sid /= from) <$> allNeighborhoodSpaces
  when (n > 0)
    $ chooseFor ctx.investigator ("Place " <> tshow n <> " doom in another space")
    $ [Choice (SpaceLabel sid) [PlaceDoomInOrder ctx.source (replicate n sid)] | sid <- spaces]

{- | "Once per round, after you gain a remnant, this item recovers one sanity."
Stated flatly, so it is not offered; nothing is spent on a journal already whole.
-}
lostJournal :: AssetBehavior
lostJournal =
  defaultAssetBehavior
    & #afterGainRemnant
    .~ \cid iid -> do
      used <- usedThisRound cid iid
      a <- use (assetL cid)
      pure [m | not used, a.horror > 0, m <- [MarkAssetUsed iid cid, RecoverAsset cid 0 1]]

{- | "Action: Test will. If you pass, exhaust a monster in your space; then you may
move that monster one space." The monster is chosen once the test has answered, and
noted on the card so the move knows which one it was.
-}
michaelLeigh :: AssetBehavior
michaelLeigh =
  defaultAssetBehavior
    & #componentActions
    .~ [ ComponentActionDef
           { label = "Michael Leigh: test will to run a monster off"
           , allowedWhileEngaged = True
           , canPerform = \iid -> do
               here <- maybe (pure []) monstersAt =<< investigatorSpace iid
               not . null <$> filterM (canBeExhausted . (.card)) here
           , perform = \ctx ->
               push
                 $ BeginTest
                   (newTest ctx.investigator Will 0 OtherTest (AfterCustom ctx.source "michael-leigh"))
           }
       ]

michaelLeighResult :: Source -> Int -> GameM ()
michaelLeighResult src r = for_ [c | SourceCard c <- [src]] \self -> do
  a <- use (assetL self)
  here <- maybe (pure []) monstersAt =<< investigatorSpace a.owner
  exhaustable <- filterM (canBeExhausted . (.card)) here
  when (r > 0)
    $ chooseFor a.owner "Exhaust a monster in your space"
    $ [ Choice
          (MonsterLabel m.card)
          [ NoteOnCard self "moved" (coerce m.card)
          , ExhaustMonster m.card
          , ResolveEffect (EffectCtx a.owner src Nothing) (Custom "michael-leigh-move")
          ]
      | m <- exhaustable
      ]

michaelLeighMove :: EffectCtx -> GameM ()
michaelLeighMove ctx = for_ [c | SourceCard c <- [ctx.source]] \self -> do
  mid <- coerce <$> cardNote self "moved"
  assetL self . #tokens .= mempty
  m <- uses #monsters (Map.lookup mid)
  board <- use #board
  for_ m \monster -> do
    name <- (.name) <$> getCardDef mid
    chooseFor ctx.investigator ("Move " <> name <> " one space?")
      $ Choice (DoneLabel "Leave it where it is") []
      : spaceChoices (monsterAdjacent monster.space board) \s -> [MoveMonsterTo mid s]

{- | "While resolving a test, after you spend a focus token to reroll a die, if you
have no focus tokens remaining, roll one additional die." Stated flatly rather than
offered.
-}
nineOfRods :: AssetBehavior
nineOfRods =
  defaultAssetBehavior
    & #afterSpentFocusToReroll
    .~ \cid iid -> do
      i <- getInvestigator iid
      pure [RollAdditionalDice (SourceCard cid) 1 | focusCount i == 0]

{- | "After you cast a spell, you may spend one remnant to remove one doom from your
space."
-}
stewardOfTheOrder :: AssetBehavior
stewardOfTheOrder =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterCastSpell iid _ -> do
        doom <- maybe (pure 0) (fmap (.doom) . getSpace) =<< investigatorSpace iid
        affordable <- canPayCost iid (SpendRemnants 1)
        pure
          [ Reaction
              "steward-of-the-order"
              "Steward of the Order: spend one remnant to remove one doom from your space"
              [ResolveEffect (cardCtx iid cid) (Pay (SpendRemnants 1) (RemoveDoomFrom YourSpace (N 1)))]
          | doom > 0
          , affordable
          ]
      _ -> pure []

{- | "After you remove one or more doom from your space, if there is no doom
remaining in your space, you or an ally may recover one sanity."
-}
theHierophant :: AssetBehavior
theHierophant =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterDoomRemoved iid removed | removed > 0 -> do
        doom <- maybe (pure 1) (fmap (.doom) . getSpace) =<< investigatorSpace iid
        let recovery = RecoverSanity YouOrAlly (N 1)
        useful <- effectUseful (cardCtx iid cid) recovery
        pure
          [ Reaction
              "the-hierophant"
              "The Hierophant: you or an ally recovers one sanity"
              [ResolveEffect (cardCtx iid cid) recovery]
          | doom == 0
          , useful
          ]
      _ -> pure []

{- | "After you become delayed, you may disengage all monsters and move directly to
the unstable space to become DRIVEN. If you do, you are no longer delayed."
-}
theRedClock :: AssetBehavior
theRedClock =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterBecomeDelayed iid -> do
        a <- use (assetL cid)
        pure
          [ Reaction
              "the-red-clock"
              "The Red Clock: slip away to the unstable space and become DRIVEN"
              [ResolveEffect (cardCtx iid cid) (Custom "the-red-clock")]
          | a.owner == iid
          ]
      _ -> pure []

theRedClockEscape :: EffectCtx -> GameM ()
theRedClockEscape ctx = do
  let iid = ctx.investigator
  ms <- engagedMonsters iid
  investigatorL iid . #delayed .= False
  pushAll
    $ [DisengageMonster iid m.card | m <- ms]
    <> [ ResolveEffect ctx (MoveDirectlyTo TheUnstableSpace)
       , ResolveEffect ctx (GainE (Condition "DRIVEN"))
       ]

{- | "Action: Place one doom in any space and test lore. An investigator in that
space may recover health and sanity, both equal to your test result." The doom goes
down before the test, so the space it went to is noted for the recovery.
-}
unknownLiturgy :: AssetBehavior
unknownLiturgy =
  defaultAssetBehavior
    & #componentActions
    .~ [ ComponentActionDef
           { label = "Unknown Liturgy: place one doom and test lore to mend"
           , allowedWhileEngaged = False
           , canPerform = \_ -> pure True
           , perform = \ctx -> push (ResolveEffect ctx (Custom "unknown-liturgy"))
           }
       ]

unknownLiturgyCast :: EffectCtx -> GameM ()
unknownLiturgyCast ctx = for_ [c | SourceCard c <- [ctx.source]] \self -> do
  spaces <- allNeighborhoodSpaces
  chooseFor ctx.investigator "Place one doom in a space"
    $ [ Choice
          (SpaceLabel sid)
          [ noteSpace self sid
          , PlaceDoomInOrder ctx.source [sid]
          , castingTest ctx self 0 (AfterCustom ctx.source "unknown-liturgy")
          ]
      | sid <- spaces
      ]

unknownLiturgyResult :: Source -> Int -> GameM ()
unknownLiturgyResult src r = for_ [c | SourceCard c <- [src]] \self -> do
  a <- use (assetL self)
  marked <- notedSpace self
  assetL self . #tokens .= mempty
  for_ marked \sid -> when (r > 0) do
    here <- investigatorsAt sid
    chooseFor a.owner ("Recover " <> tshow r <> " health and sanity")
      $ Choice (DoneLabel "Nobody") []
      : [Choice (InvestigatorLabel i.id) [RecoverInvestigator i.id r r] | i <- here]
