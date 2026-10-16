module Arkham.Homebrew.TheInvisibleMan.Acts.RunningInterference (runningInterference) where

import Arkham.Act.Import.Lifted
import Arkham.Classes.HasGame
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Homebrew.TheInvisibleMan.ScenarioDeckKeys
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype RunningInterference = RunningInterference ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

runningInterference :: ActCard RunningInterference
runningInterference =
  act (2, A) RunningInterference Cards.runningInterference
    $ Just
    $ GroupClueCost (PerPlayer 1) Anywhere

{- | Act 2f, "One Step Ahead". The printed options say to search the Invisible
*Combat* deck for Nowhere to Run and Panicked Flight, but both are Invisible
Escape cards and are only ever in the Escape deck.
-}
instance RunMessage RunningInterference where
  runMessage msg a@(RunningInterference attrs) = runQueueT $ scenarioI18n $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      lead <- getLead
      (nowhere, panicked, sharp) <- oneStepAheadState
      scope "runningInterference" $ chooseOneM lead do
        when nowhere $ labeled "removeNowhereToRun" do
          removeFromScenarioDeck InvisibleEscapeDeck Stories.nowhereToRun
        when panicked $ labeled "removePanickedFlight" do
          removeFromScenarioDeck InvisibleEscapeDeck Stories.panickedFlight
        unless sharp $ labeled "shuffleInSharpEarOut" do
          shuffleSetAsideIntoScenarioDeck InvisibleEscapeDeck (cardIs Stories.sharpEarOut)
      doStep 1 msg
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      (nowhere, panicked, sharp) <- oneStepAheadState
      if nowhere || panicked || not sharp
        then advanceToAct attrs Cards.runningInterference A
        else push $ RemoveCompletedActFromGame (actDeckId attrs) attrs.id
      pure a
    _ -> RunningInterference <$> liftRunMessage msg attrs

-- | Which of the three options are still open, in printed order.
oneStepAheadState :: HasGame m => m (Bool, Bool, Bool)
oneStepAheadState =
  (,,)
    <$> inScenarioDeck InvisibleEscapeDeck Stories.nowhereToRun
    <*> inScenarioDeck InvisibleEscapeDeck Stories.panickedFlight
    <*> inScenarioDeck InvisibleEscapeDeck Stories.sharpEarOut
