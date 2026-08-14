module Arkham.Asset.Assets.BandagesSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes.HasGame (getGame)
import Arkham.Matcher
import Arkham.Token qualified as Token
import TestImport.New

-- Assign one point of incoming damage onto an Ally asset (soak).
chooseAssignToAsset :: HasCallStack => AssetId -> TestAppT ()
chooseAssignToAsset aid = chooseOptionMatching "assign damage to asset" \case
  AssetDamageLabel aid' _ -> aid' == aid
  _ -> False

-- Assign one point of incoming damage onto the investigator.
chooseAssignToInvestigator :: HasCallStack => InvestigatorId -> TestAppT ()
chooseAssignToInvestigator iid = chooseOptionMatching "assign damage to investigator" \case
  DamageLabel iid' _ -> iid' == iid
  _ -> False

-- The targets currently offered by Bandages' "choose up to N to heal" question.
offeredHealTargets :: HasCallStack => TestAppT [Target]
offeredHealTargets = do
  questionMap <- gameQuestion <$> getGame
  case mapToList questionMap of
    [(_, question)] -> case stripQuestionWrappers question of
      ChooseUpToN _ msgs -> pure [target | TargetLabel target _ <- msgs]
      other -> error $ "expected a ChooseUpToN of heal targets, got: " <> show other
    other -> error $ "expected exactly one question, got: " <> show other

spec :: Spec
spec = describe "Bandages" do
  -- Regression for #4881: 2 damage from Dark Pact assigned 1 to the
  -- investigator and 1 to an Ally land in a single window. Bandages should be
  -- able to heal each of them (spending 1 supply per heal).
  it "heals each of multiple entities damaged in the same window" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    beatCop <- self `putAssetIntoPlay` Assets.beatCop
    bandages <- self `putAssetIntoPlay` Assets.bandages

    -- Dark Pact-style: 2 damage assigned among self and Ally in one window.
    run $ InvestigatorAssignDamage self.id (TestSource mempty) DamageAny 2 0
    chooseAssignToAsset beatCop
    chooseAssignToInvestigator self.id
    applyAllDamage

    useReaction
    chooseTarget self
    chooseTarget beatCop

    self.damage `shouldReturn` 0
    beatCop.damage `shouldReturn` 0
    bandages.uses `shouldReturn` singletonMap Token.Supply 1

  -- The user's concern: heals are per damaged *entity*, not per point of
  -- damage. 2 damage on a single investigator must offer only one heal.
  it "offers only one heal when a single investigator takes 2 damage at once" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    bandages <- self `putAssetIntoPlay` Assets.bandages

    run $ InvestigatorAssignDamage self.id (TestSource mempty) DamageAny 2 0
    applyAllDamage

    useReaction
    chooseTarget self

    self.damage `shouldReturn` 1
    bandages.uses `shouldReturn` singletonMap Token.Supply 2

  -- The number of heals is also bounded by available supplies.
  it "can only heal as many entities as it has supplies" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    beatCop <- self `putAssetIntoPlay` Assets.beatCop
    bandages <- self `putAssetIntoPlay` Assets.bandages
    -- Leave Bandages with a single supply.
    run $ SpendUses (TestSource mempty) (toTarget bandages) Token.Supply 2

    run $ InvestigatorAssignDamage self.id (TestSource mempty) DamageAny 2 0
    chooseAssignToAsset beatCop
    chooseAssignToInvestigator self.id
    applyAllDamage

    useReaction
    chooseTarget self

    self.damage `shouldReturn` 0
    beatCop.damage `shouldReturn` 1
    -- The last supply is spent, so Bandages is discarded.
    assert $ selectNone $ assetIs Assets.bandages

  -- Regression for #5394: when every point is soaked by an Ally the investigator
  -- takes no damage, so Bandages must not offer to heal damage they were already
  -- carrying. (The engine used to open a TakeDamage window on the investigator
  -- for the whole assignment, including the points that landed on the Ally.)
  it "does not offer to heal an investigator who soaked all the damage onto an Ally" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    beatCop <- self `putAssetIntoPlay` Assets.beatCop

    -- Pre-existing damage, taken before Bandages is in play so it opens no window.
    run $ InvestigatorDirectDamage self.id (TestSource mempty) 1 0
    applyAllDamage
    self.damage `shouldReturn` 1

    bandages <- self `putAssetIntoPlay` Assets.bandages

    run $ InvestigatorAssignDamage self.id (TestSource mempty) DamageAny 1 0
    chooseAssignToAsset beatCop
    applyAllDamage

    useReaction
    offeredHealTargets `shouldReturn` [AssetTarget beatCop]

    chooseTarget beatCop
    beatCop.damage `shouldReturn` 0
    self.damage `shouldReturn` 1
    bandages.uses `shouldReturn` singletonMap Token.Supply 2
