module Arkham.Homebrew.NightOfVespers.JinxSpec (spec) where

import Arkham.Act.CardDefs.NightOfTheZealot.TheGathering qualified as Acts
import Arkham.Act.Import.Lifted (ActAttrs, IsAct, act)
import Arkham.Act.Sequence (ActSide (A))
import Arkham.Act.Types (Act (..))
import Arkham.Entities qualified as Entities
import Arkham.Homebrew.NightOfVespers.CardDefs.Assets qualified as Story
import Arkham.Homebrew.NightOfVespers.Jinx
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher
import Arkham.Message.Lifted qualified as Lifted
import Test.Hspec qualified as Hspec
import TestImport.New

-- Test-only clause: the production act cards remain unimplemented. Keeping
-- this as a real ability checks windows, limits, and the triggering seat.
newtype JinxAct = JinxAct ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities JinxAct where
  getAbilities (JinxAct a) = [jinxAbility a 17]

instance RunMessage JinxAct where
  runMessage msg a@(JinxAct attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 17 windows _ -> do
      for_ (jinxContext windows) \jinxState -> resolveJinx attrs 17 jinxState $ Lifted.gainResources iid attrs 1
      pure a
    _ -> JinxAct <$> liftRunMessage msg attrs

installJinx :: TestAppT ()
installJinx = do
  card <- genCard Acts.trapped
  let built =
        cbCardBuilder
          (Act <$> act (1, A) JinxAct Acts.trapped Nothing)
          (toCardId card)
          (1, ActId (toCardCode card))
  overTest $ entitiesL . Entities.actsL %~ insertEntity built

spec :: Spec
spec = describe "Night of Vespers Jinx" do
  it "honors added and removed Jinxed keywords" do
    effectiveKeyword Jinxed mempty [AddKeyword Jinxed] `Hspec.shouldBe` True
    effectiveKeyword Jinxed (singleton Jinxed) [RemoveKeyword Jinxed] `Hspec.shouldBe` False
    effectiveKeyword Jinxed mempty [AddKeyword Jinxed, RemoveKeyword Jinxed] `Hspec.shouldBe` False
  it "finds Jinxed cards at their projected location" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    hasJinxedCardAt location.id `shouldReturn` False
    run =<< gameModifier (TestSource mempty) (toTarget location) (AddKeyword Jinxed)
    hasJinxedCardAt location.id `shouldReturn` True
  it "does nothing without a current Jinx clause" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    messages <- evalQueueT $ triggerJinx self.id
    liftIO $ messages `shouldBe` []
  it "allows direct triggers without a Jinxed card or failed test" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    installJinx
    resourcesBefore <- self.resources
    runQueueT $ triggerJinx self.id
    runMessages
    self.resources `shouldReturn` (resourcesBefore + 1)
  it "does not trigger for a plain combat test" . scenarioTest ":night-of-vespers:105" $ \self -> do
    location <- testLocation
    self `moveTo` location
    installJinx
    run =<< gameModifier (TestSource mempty) (toTarget location) (AddKeyword Jinxed)
    resourcesBefore <- self.resources
    failSkillTest self
    self.resources `shouldReturn` resourcesBefore
  it "triggers once for a failed investigation despite multiple Jinxed cards" . scenarioTest ":night-of-vespers:105" $ \self -> do
    location <- testLocation
    self `moveTo` location
    installJinx
    enemy <- testEnemy
    enemy `spawnAt` location
    run =<< gameModifier (TestSource mempty) (toTarget location) (AddKeyword Jinxed)
    run =<< gameModifier (TestSource mempty) (toTarget enemy) (AddKeyword Jinxed)
    resourcesBefore <- self.resources
    setChaosTokens [AutoFail]
    self `investigate` location
    startSkillTest
    applyResults
    self.resources `shouldReturn` (resourcesBefore + 1)
  it "includes controlled assets at their investigator's location" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    asset <- self `putAssetIntoPlay` Story.finlayODoyle
    run =<< gameModifier (TestSource mempty) (toTarget asset) (AddKeyword Jinxed)
    hasJinxedCardAt location.id `shouldReturn` True
  it "suppresses only the chosen current clause" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    installJinx
    run =<< gameModifier (TestSource mempty) (ActTarget $ ActId $ toCardCode Acts.trapped) ignoreJinx
    messages <- evalQueueT $ triggerJinx self.id
    liftIO $ messages `shouldBe` []
  it "resolves for the triggering investigator rather than every seat" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    other <- addInvestigator Investigators.rolandBanks
    other `moveTo` location
    installJinx
    ownBefore <- self.resources
    otherBefore <- other.resources
    runQueueT $ triggerJinx other.id
    runMessages
    self.resources `shouldReturn` ownBefore
    other.resources `shouldReturn` (otherBefore + 1)
  it "triggers and discards a matching Favor even without a Jinxed card" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    installJinx
    favor <- self `putAssetIntoPlay` Story.tenebrarumsFavor
    resourcesBefore <- self.resources
    setChaosTokens [Cultist]
    runQueueT $ Lifted.requestChaosTokens self.id (TestSource mempty) 1
    runMessages
    useForcedAbility
    self.resources `shouldReturn` (resourcesBefore + 1)
    assertNone $ AssetWithId favor
  it "discards a matching Favor even when no Jinx is defined" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    favor <- self `putAssetIntoPlay` Story.suspiriorumsFavor
    setChaosTokens [Tablet]
    runQueueT $ Lifted.requestChaosTokens self.id (TestSource mempty) 1
    runMessages
    useForcedAbility
    assertNone $ AssetWithId favor
  it "keeps Jinx context through a repeat" . gameTest $ \self -> do
    let jinxState = JinxContext self.id 3 True (Just (ActSource $ ActId "01108", 17))
    messages <- evalQueueT $ repeatJinx jinxState
    liftIO $ case messages of
      [UseCardAbility iid _ 17 windows _] -> do
        iid `shouldBe` self.id
        jinxContext windows `shouldBe` Just jinxState
      _ -> expectationFailure "Jinx repeat did not retain the original effect and test context"
