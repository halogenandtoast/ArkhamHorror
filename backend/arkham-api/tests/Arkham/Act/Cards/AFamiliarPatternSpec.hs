module Arkham.Act.Cards.AFamiliarPatternSpec (spec) where

import Arkham.Act (lookupAct)
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence (ActSide (..))
import Arkham.Act.Types (Act)
import Arkham.Entities qualified as Entities
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (revealedL)
import Arkham.Matcher
import Arkham.Token
import Arkham.Window (defaultWindows)
import TestImport.New

{- | Regression for #4937. "A Familiar Pattern"'s objective is a forced
@AnyWindow@ ability with @GroupClueCost (PerPlayer 2)@ (= 4 clues for two
players). When the cost is split across two investigators, paying the first
investigator's clue used to open a @SpentClues@ window *before* the clue was
actually deducted, so the still-affordable objective re-triggered mid-payment
and double-spent — leaving an orphaned sub-payment that threw
@InvalidState "Can't afford cost (b): Costs [ClueCost (Static 3)]"@.
-}
realAct :: CardDef -> TestAppT Act
realAct def = do
  card <- genCard def
  let actId' = ActId (toCardCode card)
      act' = either (error . show) id $ lookupAct actId' 1 (toCardId card)
  overTest $ entitiesL . Entities.actsL %~ insertEntity act'
  pure act'

spec :: Spec
spec = describe "A Familiar Pattern" do
  it "pays a group clue cost split across two investigators without crashing (#4937)"
    . gameTest
    $ \self -> do
      other <- addInvestigator Investigators.rolandBanks
      act <- realAct Acts.aFamiliarPattern
      -- the act's b-side spawns the Winged Serpent at the Mouth of K'n-yan
      mouth <- testLocationWithDef Locations.mouthOfKnYanTheCavernsMaw (revealedL .~ True)
      self `moveTo` mouth
      other `moveTo` mouth

      -- 3 + 1 = 4 = GroupClueCost (PerPlayer 2) for two players, so the engine
      -- takes the exact-split branch and pays each investigator in turn.
      run $ PlaceTokens (TestSource mempty) (toTarget self) Clue 3
      run $ PlaceTokens (TestSource mempty) (toTarget other) Clue 1

      [ability] <- getActionsFrom self act
      run $ UseAbility (toId self) ability (defaultWindows $ toId self)

      -- exactly 4 clues spent across the group, no double-spend, no exception,
      -- and the act actually advanced to its b-side.
      self.clues `shouldReturn` 0
      other.clues `shouldReturn` 0
      assertAny $ ActWithSide B
