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

behaviors :: Behaviors
behaviors =
  ( mempty
      & #customEffects
      .~ Map.fromList
        [ ("clover-club-gamble", gamble)
        , ("mysterious-serum", serum)
        , ("abandoned-luggage-stash", stash)
        , ("puzzle-box", openPuzzleBox)
        ]
      & #customAfterTests
      .~ Map.fromList [("abandoned-luggage", openLuggage), ("good-standing", goodStandingPrice)]
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

{- | Spend a once-a-round ability as its offer is taken rather than queueing
'MarkAbilityUsed': the turn's action prompt is asked again before the queue
unwinds, and would otherwise offer the same free action a second time.
-}
spendOncePerRound :: InvestigatorId -> Text -> GameM ()
spendOncePerRound iid key = investigatorL iid . #usedAbilities %= (<> [key])

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
