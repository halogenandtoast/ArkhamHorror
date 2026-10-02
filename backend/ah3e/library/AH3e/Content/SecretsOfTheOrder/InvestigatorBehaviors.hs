-- | The Secrets of the Order investigator sheets.
module AH3e.Content.SecretsOfTheOrder.InvestigatorBehaviors (behaviors) where

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
    { investigators =
        Map.fromList
          [ ("agatha-crane", aNewFieldOfStudy)
          , ("mark-harrigan", doggedAndSteadfast)
          , ("preston-fairmont", creatureComforts)
          , ("winifred-habbamock", justThatGood)
          ]
    , customEffects = Map.fromList [("just-that-good-penalty", justThatGoodPenalty)]
    }

sheet :: InvestigatorId -> EffectCtx
sheet iid = EffectCtx iid (SourceInvestigator iid) Nothing

-- | The card with that code this investigator holds, if they hold one.
heldCard :: InvestigatorId -> CardCode -> GameM (Maybe CardId)
heldCard iid code = do
  i <- getInvestigator iid
  listToMaybe <$> filterM (fmap (== code) . cardCode) i.assets

-- Agatha Crane -------------------------------------------------------------

{- | "A New Field of Study—After you suffer one or more horror, you may focus one
skill of your choice and gain one remnant." Only the horror that reached her
counts; a card may have soaked the rest.
-}
aNewFieldOfStudy :: InvestigatorBehavior
aNewFieldOfStudy =
  defaultInvestigatorBehavior
    & #afterHarm
    .~ \self plan -> do
      let soaked = maybe 0 snd plan.horrorTo
      pure
        [ ResolveEffect
            (sheet self)
            ( May
                "A New Field of Study: focus one skill and gain one remnant"
                (Seq [Focus Nothing False, GainE (Remnants (N 1))])
            )
        | plan.investigator == self
        , plan.horror - soaked > 0
        ]

-- Mark Harrigan ------------------------------------------------------------

{- | "Dogged—At the start of your turn, if you are delayed, you may focus one skill
of your choice or recover one sanity." and "Steadfast—You may perform the attack
action any number of times per round."

Sophie's Portrait recovers a sanity whenever Dogged is used, so the recovery is
part of what Dogged sets going; the card itself has nothing to notice it with.
-}
doggedAndSteadfast :: InvestigatorBehavior
doggedAndSteadfast =
  defaultInvestigatorBehavior
    & #repeatableActions
    .~ [AttackAction]
    & #reactions
    .~ \self -> \case
      AtStartOfTurn who | who == self -> do
        i <- getInvestigator self
        sophie <- heldCard self "sophies-portrait"
        pure
          [ Reaction
              "dogged"
              "Dogged: focus one skill or recover one sanity"
              ( ResolveEffect
                  (sheet self)
                  ( Choose
                      [ ("Focus one skill", Focus Nothing False)
                      , ("Recover one sanity", RecoverSanity You (N 1))
                      ]
                  )
                  : [RecoverAsset cid 0 1 | cid <- toList sophie]
              )
          | i.delayed
          ]
      _ -> pure []

-- Preston Fairmont ---------------------------------------------------------

{- | "Creature Comforts—At the start of your turn, you may spend $1 to recover one
health or one sanity."
-}
creatureComforts :: InvestigatorBehavior
creatureComforts =
  defaultInvestigatorBehavior
    & #reactions
    .~ \self -> \case
      AtStartOfTurn who | who == self -> do
        rich <- canPayCost self (SpendMoney 1)
        pure
          [ Reaction
              "creature-comforts"
              "Creature Comforts: spend $1 to recover one health or one sanity"
              [ ResolveEffect
                  (sheet self)
                  ( Pay
                      (SpendMoney 1)
                      ( Choose
                          [ ("Recover one health", RecoverHealth You (N 1))
                          , ("Recover one sanity", RecoverSanity You (N 1))
                          ]
                      )
                  )
              ]
          | rich
          ]
      _ -> pure []

-- Winifred Habbamock -------------------------------------------------------

{- | "Just That Good—Once per round, when you would fail a test, you may spend one
focus to roll one additional die. If you still fail that test, choose a skill
that you do not have focused; focus that skill twice."

Offered at the manipulate-dice step, which is the last moment the dice can be
changed; whether the test is failing as things stand is left to her, the way
every other "when you would fail" card is. The penalty rides on the test as a
rider, so it reads the result the test finally had rather than the one showing
when the die was bought.
-}
justThatGood :: InvestigatorBehavior
justThatGood =
  defaultInvestigatorBehavior
    & #testOptions
    .~ \self _ -> do
      used <- usedAbility self "just-that-good"
      i <- getInvestigator self
      pure
        [ Reaction
            "just-that-good"
            "Just That Good: spend one focus to roll one additional die"
            [ MarkAbilityUsed self "just-that-good"
            , PayCost (sheet self) (SpendFocus 1)
            , RollAdditionalDice (SourceInvestigator self) 1
            , AddTestRider (sheet self) (ByResult [((0, Just 0), Custom "just-that-good-penalty")])
            ]
        | not used
        , focusCount i > 0
        ]

-- | "Choose a skill that you do not have focused; focus that skill twice."
justThatGoodPenalty :: EffectCtx -> GameM ()
justThatGoodPenalty ctx = do
  let iid = ctx.investigator
  i <- getInvestigator iid
  let options = [s | s <- allSkills, Map.findWithDefault 0 s i.focus == 0]
  unless (null options)
    $ chooseFor iid "Choose a skill you do not have focused; focus it twice"
    $ [ Choice (SkillLabel s) [FocusSkill iid s False, FocusSkillAgain iid s]
      | s <- options
      ]
