module Arkham.Investigator.Cards.RexMurphyParallel (
  rexMurphyParallel,
  RexMurphyParallel (..),
)
where

import Arkham.Ability
import Arkham.Calculation
import Arkham.Helpers.ChaosBag (getRemainingCurseTokens)
import Arkham.Helpers.Window qualified as Msg
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype RexMurphyParallel = RexMurphyParallel InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

rexMurphyParallel :: InvestigatorCard RexMurphyParallel
rexMurphyParallel =
  investigator RexMurphyParallel Cards.rexMurphyParallel
    $ Stats {health = 6, sanity = 9, willpower = 3, intellect = 4, combat = 2, agility = 3}

instance HasAbilities RexMurphyParallel where
  getAbilities (RexMurphyParallel a) =
    [ restrictedAbility a 1 Self
        $ freeReaction
        $ oneOf
          [ WindowWhen (HasNRemainingCurseTokens $ atLeast 2)
              $ WouldPlaceClueOnLocation #when You YourLocation (atLeast 1)
          , WindowWhen (exists $ You <> InvestigatorAt Anywhere <> InvestigatorWithAnyClues)
              $ WouldAddChaosTokensToChaosBag #when (atLeast 2) #curse
          ]
    ]

instance HasChaosTokenValue RexMurphyParallel where
  getChaosTokenValue iid ElderSign (RexMurphyParallel attrs) | iid == toId attrs = do
    pure
      $ ChaosTokenValue ElderSign
      $ CalculatedModifier
      $ MultiplyCalculation (Fixed 2) (CountChaosTokens $ RevealedChaosTokens $ ChaosTokenFaceIs #curse)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage RexMurphyParallel where
  runMessage msg i@(RexMurphyParallel attrs) = runQueueT $ case msg of
    ElderSignEffect (is attrs -> True) -> do
      n <- selectCount $ RevealedChaosTokens $ ChaosTokenFaceIs #curse
      drawCardsIfCan attrs.id (#elderSign :: Source) n
      pure i
    UseCardAbility iid (isSource attrs -> True) 1 [w] _ -> do
      case windowType w of
        Window.WouldPlaceClueOnLocation _ _ _ n -> do
          curses <- getRemainingCurseTokens
          clues <- field InvestigatorClues iid
          let clueDiff = max 0 (n - clues)
          let mandatoryTimes = max 1 clueDiff
          let optionalTimes = max 0 $ min (n - mandatoryTimes) (curses `div` 2)
          if optionalTimes > 0
            then
              questionLabel "Number of times to use Rex's conversion" iid
                $ DropDown
                  [ (tshow x, DoStep x msg)
                  | x <- [mandatoryTimes .. (mandatoryTimes + optionalTimes)]
                  ]
            else doStep mandatoryTimes msg
        Window.WouldAddChaosTokensToChaosBag tokens -> do
          let numCurse = count (== #curse) tokens
          numClues <- field InvestigatorClues iid
          n <- getRemainingCurseTokens

          -- Let's figure out the required number of times
          -- curseDiff: how many times we need to do this in order to be consistent with tokens available, we add 1 to handle the odd case (i.e. you've asked for 3 tokens, but 0 are in the bag)
          -- mandatoryTimes: if we didn't need to curse diff, then we should do these at least once
          -- optionalTimes: If we can still do it more we will ask
          let curseDiff = max 0 ((numCurse - n + 1) `div` 2)
          let mandatoryTimes = max 1 curseDiff
          let denominator = max 0 (2 * (numClues - mandatoryTimes))
          let optionalTimes :: Int =
                if denominator == 0
                  then 0
                  else max 0 (numCurse - (2 * mandatoryTimes) + 1) `div` denominator

          if optionalTimes > 0
            then
              questionLabel "Number of times to use Rex's conversion" iid
                $ DropDown
                  [ (tshow x, DoStep x msg)
                  | x <- [mandatoryTimes .. (mandatoryTimes + optionalTimes)]
                  ]
            else doStep mandatoryTimes msg
        _ -> pure ()
      pure i
    DoStep n (UseCardAbility iid (isSource attrs -> True) 1 [w] _) -> do
      case windowType w of
        Window.WouldPlaceClueOnLocation _ lid source clues -> do
          push $ CancelBatch $ Window.getBatchId [w]
          pushAll $ replicate (n * 2) (AddChaosToken #curse)
          -- we need to check if we need to recreate the window
          let remainingClues = clues - n
          when (remainingClues > 0) do
            batchId <- getRandom
            would <-
              Msg.checkWindows
                [ (Window.mkWhen (Window.WouldPlaceClueOnLocation iid lid source remainingClues))
                    { windowBatchId = Just batchId
                    }
                ]
            push $ Would batchId [would, InvestigatorPlaceCluesOnLocation iid source remainingClues]
        Window.WouldAddChaosTokensToChaosBag tokens -> do
          push $ CancelBatch $ Window.getBatchId [w]
          push $ InvestigatorPlaceCluesOnLocation iid (attrs.ability 1) n
          -- we need to check if we need to recreate the window
          let remainingTokens = foldr deleteFirst tokens (replicate @[ChaosTokenFace] (n * 2) #curse)
          unless (null remainingTokens) do
            batchId <- getRandom
            would <-
              Msg.checkWindows
                [ (Window.mkWhen (Window.WouldAddChaosTokensToChaosBag remainingTokens))
                    { windowBatchId = Just batchId
                    }
                ]
            push $ Would batchId $ would : map AddChaosToken remainingTokens
        _ -> pure ()
      pure i
    _ -> RexMurphyParallel <$> liftRunMessage msg attrs
