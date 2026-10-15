module Arkham.Act.Cards.TheForgottenAge.TheDepthsOfYoth.JourneyToTheNexusSpec (spec) where

import Arkham.Ability (Ability, abilityCost, abilityIndex, abilitySource)
import Arkham.Act (lookupAct)
import Arkham.Act.CardDefs.TheForgottenAge.TheDepthsOfYoth qualified as Acts
import Arkham.Act.Types (Act)
import Arkham.Effect.Window (EffectWindow (EffectRoundWindow))
import Arkham.EffectMetadata (EffectMetadata (EffectModifiers))
import Arkham.Entities qualified as Entities
import Arkham.Helpers.Action (getActions)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Location.CardDefs.TheForgottenAge.TheDepthsOfYoth qualified as Locations
import Arkham.Location.Types (revealedL)
import Arkham.Token
import Arkham.Window (mkWhen)
import Arkham.Window qualified as Window
import TestImport.New

{- | Regression for #5856. "Journey to the Nexus" reads "if investigators at
Steps of Yoth /possess/ the requisite number of clues, /they/ may advance" --
the clues are not spent. Act 1b adds to the current depth level first and only
then makes each surviving investigator lose their clues, which is what lets
Steps of Yoth's reaction ("if investigators at Steps of Yoth possess 5
{perPlayer} clues as a group") fire for a second depth level.

The objective used to carry @GroupClueCost (PerPlayer 3) "Steps of Yoth"@, so 3
{perPlayer} clues were deducted before the @AddingToCurrentDepth@ window opened
and the rope reward needed 8 {perPlayer} clues -- more than the scenario can
produce.
-}
realAct :: CardDef -> TestAppT Act
realAct def = do
  card <- genCard def
  let actId' = ActId (toCardCode card)
      act' = either (error . show) id $ lookupAct actId' 1 (toCardId card)
  overTest $ entitiesL . Entities.actsL %~ insertEntity act'
  pure act'

endRoundObjective :: Investigator -> Act -> TestAppT (Maybe Ability)
endRoundObjective self act =
  find (\ab -> abilitySource ab == toSource act && abilityIndex ab == 2)
    <$> getActions (toId self) [mkWhen Window.AtEndOfRound]

cannotSpendClues :: Investigator -> TestAppT ()
cannotSpendClues i = do
  mods <- toModifiers (TestSource mempty) [CannotSpendClues]
  run
    $ CreateWindowModifierEffect
      EffectRoundWindow
      (EffectModifiers mods)
      (TestSource mempty)
      (toTarget i)

spec :: Spec
spec = describe "Journey to the Nexus" do
  it "is not offered below 3 clues per investigator" . gameTest $ \self -> do
    other <- addInvestigator Investigators.rolandBanks
    steps <- testLocationWithDef Locations.stepsOfYoth (revealedL .~ True)
    self `moveTo` steps
    other `moveTo` steps
    act <- realAct Acts.journeyToTheNexus

    -- two investigators, so PerPlayer 3 is 6
    run $ PlaceTokens (TestSource mempty) (toTarget self) Clue 3
    run $ PlaceTokens (TestSource mempty) (toTarget other) Clue 2

    endRoundObjective self act `shouldReturn` Nothing

  it "is offered when investigators at Steps of Yoth possess 3 clues per investigator"
    . gameTest
    $ \self -> do
      other <- addInvestigator Investigators.rolandBanks
      steps <- testLocationWithDef Locations.stepsOfYoth (revealedL .~ True)
      self `moveTo` steps
      other `moveTo` steps
      act <- realAct Acts.journeyToTheNexus

      run $ PlaceTokens (TestSource mempty) (toTarget self) Clue 4
      run $ PlaceTokens (TestSource mempty) (toTarget other) Clue 2

      endRoundObjective self act `shouldSatisfyM` isJust

  it "possesses the clues rather than spending them (#5856)" . gameTest $ \self -> do
    other <- addInvestigator Investigators.rolandBanks
    steps <- testLocationWithDef Locations.stepsOfYoth (revealedL .~ True)
    self `moveTo` steps
    other `moveTo` steps
    act <- realAct Acts.journeyToTheNexus

    run $ PlaceTokens (TestSource mempty) (toTarget self) Clue 4
    run $ PlaceTokens (TestSource mempty) (toTarget other) Clue 2

    -- Nothing is spent, so an effect that forbids spending clues cannot take
    -- the advance away. With the old GroupClueCost the objective vanished here.
    cannotSpendClues self
    cannotSpendClues other

    mobjective <- endRoundObjective self act
    case mobjective of
      Nothing -> liftIO $ expectationFailure "expected the end-of-round objective"
      Just objective -> liftIO $ abilityCost objective `shouldBe` Free

  it "ignores clues held away from Steps of Yoth" . gameTest $ \self -> do
    other <- addInvestigator Investigators.rolandBanks
    steps <- testLocationWithDef Locations.stepsOfYoth (revealedL .~ True)
    elsewhere <- testLocation
    self `moveTo` steps
    other `moveTo` elsewhere
    act <- realAct Acts.journeyToTheNexus

    run $ PlaceTokens (TestSource mempty) (toTarget self) Clue 2
    run $ PlaceTokens (TestSource mempty) (toTarget other) Clue 4

    endRoundObjective self act `shouldReturn` Nothing

  it "is only offered to an investigator at Steps of Yoth" . gameTest $ \self -> do
    other <- addInvestigator Investigators.rolandBanks
    steps <- testLocationWithDef Locations.stepsOfYoth (revealedL .~ True)
    elsewhere <- testLocation
    self `moveTo` elsewhere
    other `moveTo` steps
    act <- realAct Acts.journeyToTheNexus

    run $ PlaceTokens (TestSource mempty) (toTarget other) Clue 6

    -- the left-behind investigator does not get to defeat the party
    endRoundObjective self act `shouldReturn` Nothing
    endRoundObjective other act `shouldSatisfyM` isJust
