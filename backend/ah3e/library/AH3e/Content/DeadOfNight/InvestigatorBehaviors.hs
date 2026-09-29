-- | The Dead of Night investigator sheets.
module AH3e.Content.DeadOfNight.InvestigatorBehaviors (behaviors) where

import AH3e.Engine.Behavior
import AH3e.Engine.Query
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Effect
import AH3e.Types.State
import Data.Map.Strict qualified as Map

behaviors :: Behaviors
behaviors =
  mempty
    { investigators =
        Map.fromList
          [ ("roland-banks", fightForTheTruth)
          , ("skids-otoole", wontGoBack)
          , ("kate-winthrop", seeTheWholePicture)
          , ("diana-stanley", forbiddenPractices)
          ]
    }

{- | Roland Banks. "As part of an attack action, you may add one to the result of a
number of dice equal to the number of clues in your neighborhood." The dice are
raised one at a time, so the offer is simply repeated that many times.
-}
fightForTheTruth :: InvestigatorBehavior
fightForTheTruth =
  defaultInvestigatorBehavior
    & #testOptions
    .~ \iid ts -> case ts.kind of
      ActionTest AttackAction _ | liveDiceCount ts > 0 -> do
        clues <- maybe (pure 0) (fmap (.clues) . getNeighborhood) =<< investigatorNeighborhood iid
        used <- usedAbility iid "fight-for-the-truth"
        pure
          [ Reaction
              "fight-for-the-truth"
              ("Fight for the Truth: add one to " <> tshow clues <> " dice")
              ( MarkAbilityUsed iid "fight-for-the-truth"
                  : replicate clues (AddToDie (SourceInvestigator iid))
              )
          | clues > 0 && not used
          ]
      _ -> pure []

{- | "Skids" O'Toole. "Once per round, after a test in which you rolled a 1, you may
focus one skill of your choice, even if it exceeds your focus limit." Offered
while the 1 is still on the table, which is the only moment it can be seen.
-}
wontGoBack :: InvestigatorBehavior
wontGoBack =
  defaultInvestigatorBehavior
    & #testOptions
    .~ \iid ts -> do
      used <- usedAbility iid "wont-go-back"
      let rolledOne = any (\d -> d.value == 1) ts.dice
      pure
        [ Reaction
            "wont-go-back"
            "Won't Go Back: focus one skill, even beyond your limit"
            [ MarkAbilityUsed iid "wont-go-back"
            , ResolveEffect (EffectCtx iid (SourceInvestigator iid) Nothing) (Focus Nothing True)
            ]
        | rolledOne && not used
        ]

{- | Kate Winthrop. "After you perform a research action, you may focus a number of
skills of your choice equal to your test result." The result is the research
test's, whether or not there were clues to move.
-}
seeTheWholePicture :: InvestigatorBehavior
seeTheWholePicture =
  defaultInvestigatorBehavior
    & #reactions
    .~ \iid -> \case
      AfterResearchResult who n
        | who == iid
        , n > 0 ->
            pure
              [ Reaction
                  "see-the-whole-picture"
                  ("See the Whole Picture: focus " <> tshow n <> " skills")
                  [ ResolveEffect
                      (EffectCtx iid (SourceInvestigator iid) Nothing)
                      (Seq (replicate n (Focus Nothing False)))
                  ]
              ]
      _ -> pure []

{- | Diana Stanley. "When you would suffer horror (including direct horror), you may
place up to two doom in your space to prevent an equal amount of that horror."
One offer per doom, so she can stop at one.
-}
forbiddenPractices :: InvestigatorBehavior
forbiddenPractices =
  defaultInvestigatorBehavior
    & #damagePrevention
    .~ \iid plan ->
      pure
        [ Reaction
            ("forbidden-practices-" <> tshow k)
            ("Forbidden Practices: place " <> tshow k <> " doom to prevent " <> tshow k <> " horror")
            [ ResolveEffect (EffectCtx iid (SourceInvestigator iid) Nothing) (PlaceDoomAt YourSpace (N k))
            , PreventedHarm 0 k
            ]
        | plan.investigator == iid
        , plan.horror > 0
        {- The prevention step offers one reaction at a time and declines the key it
        showed, so the amounts run downwards: take two, or skip to be offered one. -}
        , k <- reverse [1 .. min 2 plan.horror]
        ]
