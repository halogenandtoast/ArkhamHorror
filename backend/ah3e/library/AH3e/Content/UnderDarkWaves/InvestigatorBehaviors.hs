-- | The Under Dark Waves investigator sheets.
module AH3e.Content.UnderDarkWaves.InvestigatorBehaviors (behaviors) where

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
    { investigators =
        Map.fromList
          [ ("carson-sinclair", quietlyIndispensable)
          , ("charlie-kane", taskForce)
          , ("father-mateo", mementoMori)
          , ("patrice-hathaway", throughTheEyesOfTheWatcher)
          , ("silas-marsh", taintedBlood)
          , ("stella-clark", onTheJob)
          , ("zoey-samaras", graceOfStGeorge)
          ]
    , customEffects = Map.fromList [("task-force", recruit)]
    }

-- Carson Sinclair ----------------------------------------------------------

{- | "Once per round, while an investigator in any space is performing a test, you
may spend one focus token to allow them to reroll one die. If you do, you recover
one sanity." His own test reaches him through the sheet; anyone else's reaches him
through the window their roll opens, since the focus spent is his.
-}
quietlyIndispensable :: InvestigatorBehavior
quietlyIndispensable =
  defaultInvestigatorBehavior
    & #testOptions
    .~ carsonOffer
    & #reactions
    .~ \self -> \case
      AnotherResolvesTest owner _
        | owner == self ->
            use #test >>= maybe (pure []) (carsonOffer self)
      _ -> pure []

carsonOffer :: InvestigatorId -> TestState -> GameM [Reaction]
carsonOffer self ts = do
  used <- usedAbility self "quietly-indispensable"
  i <- getInvestigator self
  let ctx = EffectCtx self (SourceInvestigator self) Nothing
  pure
    [ Reaction
        "quietly-indispensable"
        "Quietly Indispensable: spend a focus so they may reroll a die"
        -- paying asks which focus; its answer runs before the rest of this list
        [ MarkAbilityUsed self "quietly-indispensable"
        , PayCost ctx (SpendFocus 1)
        , RerollUpTo (SourceInvestigator self) 1
        , RecoverInvestigator self 0 1
        ]
    | not used
    , focusCount i > 0
    , liveDiceCount ts > 0
    ]

-- Charlie Kane -------------------------------------------------------------

{- | "Task Force—Action: Spend $2, and an additional $1 for each ally you have, to
gain one ally." plus "Leadership—Each time one of your allies is discarded, suffer
one direct horror." His focus limit is counted from his allies, which
'AH3e.Engine.Query.focusLimit' reads off the sheet's name.
-}
taskForce :: InvestigatorBehavior
taskForce =
  defaultInvestigatorBehavior
    & #componentActions
    .~ [ ComponentActionDef
           { label = "Task Force: recruit an ally"
           , allowedWhileEngaged = False
           , canPerform = \iid -> do
               price <- allyPrice iid
               i <- getInvestigator iid
               deck <- use (#decks . #ally)
               pure (i.money >= price && not (null deck))
           , perform = \ctx -> push (ResolveEffect ctx (Custom "task-force"))
           }
       ]
    & #onOwnedDiscard
    .~ \self cid -> do
      ally <- cardMatches AllyCard cid
      pure [SufferHarm self (SourceInvestigator self) DirectHarm 0 1 | ally]

-- | \$2, and another dollar for every ally already on the payroll.
allyPrice :: InvestigatorId -> GameM Int
allyPrice iid = (2 +) . length <$> matchingAssets iid AllyCard

-- | The price is read when the action is taken, not when the sheet was printed.
recruit :: EffectCtx -> GameM ()
recruit ctx = do
  price <- allyPrice ctx.investigator
  push (ResolveEffect ctx (Pay (SpendMoney price) (GainE (AnAlly Nothing))))

-- Father Mateo -------------------------------------------------------------

{- | "Memento Mori—Once per round, while resolving a test, you may spend one remnant
to reroll one or all of your dice." plus "Blood of Martyrs—After you suffer one or
more damage, you gain one remnant."
-}
mementoMori :: InvestigatorBehavior
mementoMori =
  defaultInvestigatorBehavior
    & #testOptions
    .~ ( \self ts -> do
           used <- usedAbility self "memento-mori"
           i <- getInvestigator self
           let live = liveDiceCount ts
               ctx = EffectCtx self (SourceInvestigator self) Nothing
               offer key lbl finish =
                 Reaction
                   key
                   lbl
                   [MarkAbilityUsed self "memento-mori", PayCost ctx (SpendRemnants 1), finish]
           pure
             [ r
             | not used
             , i.remnants > 0
             , live > 0
             , r <-
                 [ offer "memento-mori-one" "Memento Mori: spend a remnant to reroll one die"
                     $ RerollUpTo (SourceInvestigator self) 1
                 , offer "memento-mori-all" "Memento Mori: spend a remnant to reroll all your dice"
                     $ RerollAll (SourceInvestigator self)
                 ]
             ]
       )
    & #afterHarm
    .~ \self plan -> do
      -- only the damage that reached him counts; a card may have soaked the rest
      let soaked = maybe 0 snd plan.damageTo
      pure [GainRemnants self 1 | plan.investigator == self, plan.damage - soaked > 0]

-- Patrice Hathaway ---------------------------------------------------------

{- | "Each time doom is placed on or moved to the scenario sheet, you may focus one
skill of your choice, even if it exceeds your focus limit."
-}
throughTheEyesOfTheWatcher :: InvestigatorBehavior
throughTheEyesOfTheWatcher =
  defaultInvestigatorBehavior
    & #reactions
    .~ \self -> \case
      AfterDoomOnSheet who _
        | who == self ->
            pure
              [ Reaction
                  "through-the-eyes-of-the-watcher"
                  "Through the Eyes of the Watcher: focus one skill, even beyond your limit"
                  [ ResolveEffect
                      (EffectCtx self (SourceInvestigator self) Nothing)
                      (Focus Nothing True)
                  ]
              ]
      _ -> pure []

-- Silas Marsh --------------------------------------------------------------

{- | "Once per round, when a ready monster activates, you may choose any investigator
to be its prey. If that monster engages you, you may recover two sanity." The
choice is offered as one option per investigator, since the activation is already
asking the table what to do about this monster.
-}
taintedBlood :: InvestigatorBehavior
taintedBlood =
  defaultInvestigatorBehavior
    & #replacesActivation
    .~ ( \self mid -> do
           used <- usedAbility self "tainted-blood"
           ready <- isMonsterReady mid
           invs <- playingInvestigators
           name <- (.name) <$> getCardDef mid
           who <- for invs \i -> (,) i.id . (.name) <$> getInvestigatorDef i.id
           pure
             [ Reaction
                 ("tainted-blood-" <> coerce iid)
                 ("Tainted Blood: make " <> nm <> " " <> name <> "'s prey")
                 [MarkAbilityUsed self "tainted-blood", SetMonsterPrey mid iid, DoActivateMonster mid]
             | not used
             , ready
             , (iid, nm) <- who
             ]
       )
    & #reactions
    .~ \self -> \case
      -- the blood only calls back the monster he steered, which is the one he named a prey for
      AfterEngaged who mid | who == self -> do
        steered <- uses #monsters (maybe False (isJust . (.prey)) . Map.lookup mid)
        pure
          [ Reaction
              "tainted-blood-recover"
              "Tainted Blood: recover two sanity"
              [RecoverInvestigator self 0 2]
          | steered
          ]
      _ -> pure []

-- Stella Clark -------------------------------------------------------------

{- | "On the Job—At the beginning of your first action phase of the game, you may move
directly to any space." plus "Delivery Route—After you move three or more spaces
during a single round, focus one skill of your choice." The route is printed
without a "may", but every sheet hook on a move offers rather than compels, so it
comes with a skip the card does not.
-}
onTheJob :: InvestigatorBehavior
onTheJob =
  defaultInvestigatorBehavior
    & #reactions
    .~ \self -> \case
      AtStartOfTurn who | who == self -> do
        r <- use #round
        pure
          [ Reaction
              "on-the-job"
              "On the Job: move directly to any space"
              [ ResolveEffect
                  (EffectCtx self (SourceInvestigator self) Nothing)
                  (MoveDirectlyTo AnySpace)
              ]
          | r <= 1
          ]
      AfterMoveDistance who _ | who == self -> do
        used <- usedAbility self "delivery-route"
        i <- getInvestigator self
        pure
          [ Reaction
              "delivery-route"
              "Delivery Route: focus one skill"
              [ MarkAbilityUsed self "delivery-route"
              , ResolveEffect
                  (EffectCtx self (SourceInvestigator self) Nothing)
                  (Focus Nothing False)
              ]
          | not used
          , i.spacesMovedThisRound >= 3
          ]
      _ -> pure []

-- Zoey Samaras -------------------------------------------------------------

{- | "Grace of St. George—After you defeat a monster, you may remove one doom from
your space or recover one health." plus "Answer the Call—After you defeat a monster
with elite, become BLESSED." The monster is still on the board here, so its card
can still be read.
-}
graceOfStGeorge :: InvestigatorBehavior
graceOfStGeorge =
  defaultInvestigatorBehavior
    & #afterMonsterDefeated
    .~ \self mid src -> do
      mine <- defeatedBy self src
      d <- monsterDef mid
      let ctx = EffectCtx self (SourceInvestigator self) Nothing
      pure
        $ [ ResolveEffect
              ctx
              ( May
                  "Grace of St. George"
                  ( Choose
                      [ ("Remove one doom from your space", RemoveDoomFrom YourSpace (N 1))
                      , ("Recover one health", RecoverHealth You (N 1))
                      ]
                  )
              )
          | mine
          ]
        <> [GainConditionMsg self "BLESSED" | mine, d.elite > 0]

-- | Whether this investigator is the one who finished it, by their own hand or a card's.
defeatedBy :: InvestigatorId -> Source -> GameM Bool
defeatedBy self = \case
  SourceInvestigator iid -> pure (iid == self)
  SourceCard cid -> uses #assets (maybe False ((== self) . (.owner)) . Map.lookup cid)
  _ -> pure False
