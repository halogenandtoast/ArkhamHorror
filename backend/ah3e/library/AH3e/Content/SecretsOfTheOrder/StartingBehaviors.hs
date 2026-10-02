-- | What the Secrets of the Order investigators' own cards do.
module AH3e.Content.SecretsOfTheOrder.StartingBehaviors (behaviors) where

import AH3e.Engine.Behavior
import AH3e.Engine.Helpers
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.State
import Data.Map.Strict qualified as Map

behaviors :: Behaviors
behaviors =
  mempty
    { assets =
        Map.fromList
          [ ("occult-principle", occultPrinciple)
          , ("call-the-dead", callTheDeadCard)
          , ("spirit-camera", spiritCamera)
          , ("one-man-army", oneManArmy)
          , ("sophies-portrait", sophiesPortrait)
          , ("war-of-attrition", warOfAttrition)
          , ("family-inheritance", familyInheritance)
          , ("money-talks", moneyTalks)
          , ("life-of-privilege", lifeOfPrivilege)
          , ("barnstormer", barnstormer)
          , ("anything-you-can-do", anythingYouCanDo)
          , ("reckless-resolve", recklessResolve)
          ]
    , customEffects =
        Map.fromList
          [ ("flip-talent", flipTalent)
          , ("call-the-dead", callTheDead)
          , ("family-inheritance", familyInheritanceReckoning)
          , ("anything-you-can-do", anythingYouCanDoPool)
          ]
    }

sourceCard :: EffectCtx -> Maybe CardId
sourceCard ctx = case ctx.source of
  SourceCard cid -> Just cid
  _ -> Nothing

cardCtx :: InvestigatorId -> CardId -> EffectCtx
cardCtx iid cid = EffectCtx iid (SourceCard cid) Nothing

-- | How much of that pile a card is keeping on itself.
noted :: CardId -> Text -> GameM Int
noted cid key = uses #assets (Map.findWithDefault 0 key . maybe mempty (.tokens) . Map.lookup cid)

-- | "Flip this talent", for a double-sided talent whose two sides are one card.
flipTalent :: EffectCtx -> GameM ()
flipTalent ctx = for_ (sourceCard ctx) \cid -> do
  assetL cid . #flipped %= not
  a <- use (assetL cid)
  d <- getCardDef cid
  logText (d.name <> (if a.flipped then " turns over" else " turns back over"))

-- Agatha Crane's cards -----------------------------------------------------

{- | Occult Principle, and the Scientific Method on its back. Each side buys its
own advantage by turning the card over, so only one of them is ever on offer:
the principle trades doom for a second clue, and the method trades a strong ward
for a free research action.
-}
occultPrinciple :: AssetBehavior
occultPrinciple =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterGainNeighborhoodClue iid -> do
        a <- use (assetL cid)
        pure
          [ Reaction
              "occult-principle"
              "Occult Principle: place one doom in your space for one more clue"
              [ ResolveEffect (cardCtx iid cid) (PlaceDoomAt YourSpace (N 1))
              , ResolveEffect (cardCtx iid cid) (GainE (Clues (N 1)))
              , ResolveEffect (cardCtx iid cid) (Custom "flip-talent")
              ]
          | a.owner == iid
          , not a.flipped
          ]
      AfterWardResult iid result -> do
        a <- use (assetL cid)
        -- 470.2: researching moves your own clues onto the sheet, so it needs one
        i <- getInvestigator iid
        pure
          [ Reaction
              "scientific-method"
              "Scientific Method: research as an additional action"
              [ PerformGrantedAction iid ResearchAction False
              , ResolveEffect (cardCtx iid cid) (Custom "flip-talent")
              ]
          | a.owner == iid
          , a.flipped
          , result >= 2
          , i.clues > 0
          ]
      _ -> pure []

{- | "Call the Dead. Action: Test lore -1. If you pass, discard the top card of your
location's encounter deck. If you discard an event this way, gain one clue from
your neighborhood."
-}
callTheDeadCard :: AssetBehavior
callTheDeadCard = spellAction "Call the Dead" (-1) (Custom "call-the-dead")

{- | The top card of the deck the caller would draw from where they stand. A
discarded event card goes to the event discard, which is where an event whose
clue has been taken goes; anything else goes under its own deck, there being no
other pile for an encounter card.
-}
callTheDead :: EffectCtx -> GameM ()
callTheDead ctx = do
  mnid <- investigatorNeighborhood ctx.investigator
  discardTop (encounterDeckLens mnid)
 where
  discardTop :: Lens' Game [CardId] -> GameM ()
  discardTop l =
    use l >>= \case
      [] -> logText "The dead have nothing left to say"
      (cid : rest) -> do
        d <- getCardDef cid
        logText ("The dead give up " <> d.name)
        case d.kind of
          EventCard _ -> do
            l .= rest
            #decks . #eventDiscard %= (cid :)
            push (ResolveEffect ctx (GainE ClueFromNeighborhood))
          _ -> l .= rest <> [cid]

{- | "Spirit Camera: once per round, when you would draw and resolve a mythos token,
you may spend two remnants to spawn a clue instead. Place that clue's event card
on top of its encounter deck."
-}
spiritCamera :: AssetBehavior
spiritCamera =
  defaultAssetBehavior
    & #replacesMythosDraw
    .~ \cid iid -> do
      used <- usedThisRound cid iid
      affordable <- canPayCost iid (SpendRemnants 2)
      pure
        [ Reaction
            "spirit-camera"
            "Spirit Camera: spend two remnants to spawn a clue instead"
            [ MarkAssetUsed iid cid
            , PayCost (cardCtx iid cid) (SpendRemnants 2)
            , SpawnClueOnTop
            ]
        | not used
        , affordable
        ]

-- Mark Harrigan's cards ----------------------------------------------------

{- | "One Man Army: after you end your movement in a monster's space, or after a
monster moves into or spawns in your space, you may become delayed to perform an
attack action as an additional action."
-}
oneManArmy :: AssetBehavior
oneManArmy =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterMoveAction iid -> do
        here <- maybe (pure []) monstersAt =<< investigatorSpace iid
        offerAttack cid iid (not (null here))
      AfterMonsterArrives iid _ -> offerAttack cid iid True
      _ -> pure []
 where
  offerAttack cid iid there = do
    a <- use (assetL cid)
    pure
      [ Reaction
          "one-man-army"
          "One Man Army: become delayed to attack as an additional action"
          [ PayCost (cardCtx iid cid) CostDelayed
          , PerformGrantedAction iid AttackAction False
          ]
      | a.owner == iid
      , there
      ]

{- | "Sophie's Portrait: once per round, while resolving a test, you may suffer one
damage to reroll one die or all dice." The sanity it recovers when Dogged is used
is handed out by Dogged itself, the portrait having nothing to notice it with.
-}
sophiesPortrait :: AssetBehavior
sophiesPortrait =
  defaultAssetBehavior
    & #testOptions
    .~ \cid iid ts -> do
      used <- usedThisRound cid iid
      let offer key lbl ms =
            Reaction
              ("sophies-portrait" <> key)
              ("Sophie's Portrait: suffer one damage to " <> lbl)
              ( MarkAssetUsed iid cid
                  : SufferHarm iid (SourceCard cid) NormalHarm 1 0
                  : ms
              )
      pure
        [ o
        | not used
        , liveDiceCount ts > 0
        , o <-
            [ offer "-one" "reroll one die" [RerollUpTo (SourceCard cid) 1]
            , offer "-all" "reroll all dice" [RerollAll (SourceCard cid)]
            ]
        ]

{- | "War of Attrition: at the end of your turn, you may suffer any amount of direct
damage to deal damage equal to one less than that amount to each monster engaged
with you. (You cannot voluntarily suffer damage in excess of your health.)"

One offer per amount, counted down from everything they have left; one damage
would deal none, so the offers stop short of it.
-}
warOfAttrition :: AssetBehavior
warOfAttrition =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AtEndOfTurn iid -> do
        a <- use (assetL cid)
        i <- getInvestigator iid
        most <- investigatorHealth iid
        engaged <- map (.card) <$> engagedMonsters iid
        let spare = most - i.damage
        pure
          [ Reaction
              ("war-of-attrition-" <> tshow k)
              ( "War of Attrition: suffer "
                  <> tshow k
                  <> " direct damage to deal "
                  <> tshow (k - 1)
                  <> " to each monster on you"
              )
              ( SufferHarm iid (SourceCard cid) DirectHarm k 0
                  : [DealMonsterDamage mid (SourceCard cid) (k - 1) | mid <- engaged]
              )
          | a.owner == iid
          , not (null engaged)
          , k <- reverse [2 .. spare]
          ]
      _ -> pure []

-- Preston Fairmont's cards -------------------------------------------------

{- | "Family Inheritance. Reckoning—Gain $1 and all of the money on this card. After
you perform a gather resources action, place an additional $2 on this card. (You
cannot spend, use, or trade money on this card.)"

The money sits on the card as a note rather than as its owner's, which is what
keeps it out of reach until the reckoning hands it over.
-}
familyInheritance :: AssetBehavior
familyInheritance =
  defaultAssetBehavior
    & #reckoning
    ?~ Custom "family-inheritance"
    & #afterOwnerAction
    .~ \cid iid kind -> do
      a <- use (assetL cid)
      held <- noted cid "money"
      pure [NoteOnCard cid "money" (held + 2) | a.owner == iid, kind == GatherResourcesAction]

familyInheritanceReckoning :: EffectCtx -> GameM ()
familyInheritanceReckoning ctx = for_ (sourceCard ctx) \cid -> do
  held <- noted cid "money"
  assetL cid . #tokens . at "money" ?= 0
  addMoney ctx.investigator (1 + held)
  logText ("The family money comes through: $" <> tshow (1 + held))

{- | "Money Talks: while you are resolving a test, you may spend $1 to reroll one
die. (You can use this talent any number of times per test.)" Nothing marks it
used, so the offer comes back for as long as the money lasts.
-}
moneyTalks :: AssetBehavior
moneyTalks =
  defaultAssetBehavior
    & #testOptions
    .~ \cid iid ts -> do
      affordable <- canPayCost iid (SpendMoney 1)
      pure
        [ Reaction
            "money-talks"
            "Money Talks: spend $1 to reroll one die"
            [ PayCost (cardCtx iid cid) (SpendMoney 1)
            , SpendForReroll (FreeReroll (SourceCard cid))
            ]
        | affordable
        , liveDiceCount ts > 0
        ]

{- | "Life of Privilege: after you focus a skill as part of a focus action, you may
spend $1 to focus that skill one additional time."
-}
lifeOfPrivilege :: AssetBehavior
lifeOfPrivilege =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AfterFocusedSkill iid skill -> do
        a <- use (assetL cid)
        affordable <- canPayCost iid (SpendMoney 1)
        pure
          [ Reaction
              "life-of-privilege"
              "Life of Privilege: spend $1 to focus that skill again"
              [PayCost (cardCtx iid cid) (SpendMoney 1), FocusSkillAgain iid skill]
          | a.owner == iid
          , affordable
          ]
      _ -> pure []

-- Winifred Habbamock's cards -----------------------------------------------

{- | "Barnstormer: once per round, when you would pass a test, you may reroll all
dice. If you do, become DRIVEN. If you are already DRIVEN, you may recover one
health or one sanity instead."

Offered at the manipulate-dice step, the last moment the dice can be changed;
whether the test is passing as things stand is left to her, as it is for every
other card worded this way.
-}
barnstormer :: AssetBehavior
barnstormer =
  defaultAssetBehavior
    & #testOptions
    .~ \cid iid ts -> do
      used <- usedThisRound cid iid
      driven <- hasCondition iid "DRIVEN"
      let payment
            | driven =
                ResolveEffect
                  (cardCtx iid cid)
                  ( May
                      "Barnstormer: recover one health or one sanity"
                      ( Choose
                          [ ("Recover one health", RecoverHealth You (N 1))
                          , ("Recover one sanity", RecoverSanity You (N 1))
                          ]
                      )
                  )
            | otherwise = GainConditionMsg iid "DRIVEN"
      pure
        [ Reaction
            "barnstormer"
            "Barnstormer: reroll all dice"
            [MarkAssetUsed iid cid, payment, RerollAll (SourceCard cid)]
        | not used
        , liveDiceCount ts > 0
        ]

{- | "Anything You Can Do: after an investigator in any space performs an action, you
may become delayed to perform that same action. If that action requires a test,
roll one more die than they did, instead of your normal dice pool. (Normal action
restrictions still apply.)"

Their pool is noted on the card and read back by the effect that sets hers, since
the count is known now and the test she rolls has not begun. An action nobody
rolled for leaves the count at nothing, and then she rolls her own pool.
-}
anythingYouCanDo :: AssetBehavior
anythingYouCanDo =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      AnotherPerformsAction self other kind -> do
        a <- use (assetL cid)
        theirs <- fromMaybe 0 . (.lastTestDice) <$> getInvestigator other
        pure
          [ Reaction
              "anything-you-can-do"
              "Anything You Can Do: become delayed to do the same"
              ( [PayCost (cardCtx self cid) CostDelayed]
                  <> [ NoteOnCard cid "pool" (theirs + 1)
                     | theirs > 0
                     ]
                  <> [ ResolveEffect (cardCtx self cid) (Custom "anything-you-can-do")
                     | theirs > 0
                     ]
                  <> [PerformGrantedAction self kind False]
              )
          | a.owner == self
          ]
      _ -> pure []

{- | The pool their test will roll, taken off the card and handed to the next test
she begins.
-}
anythingYouCanDoPool :: EffectCtx -> GameM ()
anythingYouCanDoPool ctx = for_ (sourceCard ctx) \cid -> do
  pool <- noted cid "pool"
  assetL cid . #tokens . at "pool" .= Nothing
  when (pool > 0) $ investigatorL ctx.investigator . #fixedPoolNext ?= pool

{- | "Reckless Resolve: after you roll dice, you may become delayed to roll one
additional die for each die that is not a success." What counts as a success is
the engine's business, so the dice are counted there.
-}
recklessResolve :: AssetBehavior
recklessResolve =
  defaultAssetBehavior
    & #testOptions
    .~ \cid iid ts ->
      pure
        [ Reaction
            "reckless-resolve"
            "Reckless Resolve: become delayed to roll another die for each failing die"
            [ PayCost (cardCtx iid cid) CostDelayed
            , RollADiePerFailure (SourceCard cid)
            ]
        | liveDiceCount ts > 0
        ]
