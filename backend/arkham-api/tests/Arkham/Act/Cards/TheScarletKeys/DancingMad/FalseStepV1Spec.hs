module Arkham.Act.Cards.TheScarletKeys.DancingMad.FalseStepV1Spec (spec) where

import Arkham.Act (lookupAct)
import Arkham.Act.CardDefs.TheScarletKeys.DancingMad qualified as Acts
import Arkham.Act.Types (Act)
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (AssetCard))
import Arkham.Campaigns.TheScarletKeys.Meta (initMeta)
import Arkham.Classes.HasGame (getGame)
import Arkham.Entities qualified as Entities
import Arkham.Helpers.Scenario
import Arkham.Matcher hiding (AssetCard)
import Arkham.Projection (field)
import Arkham.Scenario.Types (Field (..))
import Helpers.Achievements (asTheScarletKeysScenario)
import TestImport.New

{- | False Step (v. I) sets Desiderio aside out of play, both when he would leave
play (ability 2, which cancels the normal leave-play batch) and when the act
advances. Neither path removed the asset entity, so he sat in the play area and
in the set aside pile at once -- the same defect as the Hollow in issue #6041.
-}
spec :: Spec
spec = describe "False Step (v. I)" do
  it "takes Desiderio out of play when he would leave play" . gameTest $ \self -> do
    -- Desiderio reads the campaign's key statuses as he enters play
    asTheScarletKeysScenario "09591"
    run $ SetCampaignMeta (toJSON initMeta)
    location <- testLocation
    self `moveTo` location
    _ <- realAct Acts.falseStepV1
    desi <- self `putAssetIntoPlay` Assets.desiderioDelgadoAlvarez
    card <- field AssetCard desi

    run $ Discard Nothing (TestSource mempty) (AssetTarget desi)
    -- the Forced ability parks on a trigger prompt in a spec
    resolveForcedAbility

    assetIsGone desi
    setAsideHas card

  it "takes Desiderio out of play as the act advances" . gameTest $ \self -> do
    asTheScarletKeysScenario "09591"
    run $ SetCampaignMeta (toJSON initMeta)
    location <- testLocation
    self `moveTo` location
    act <- realAct Acts.falseStepV1
    desi <- self `putAssetIntoPlay` Assets.desiderioDelgadoAlvarez
    card <- field AssetCard desi

    run $ AdvanceAct (toId act) (TestSource mempty) AdvancedWithOther
    chooseOnlyOption "advance to side B"

    selectAny (assetIs Assets.desiderioDelgadoAlvarez) `shouldReturn` False
    setAsideHas card

{- | The live entity map, not a matcher: cancelling the leave-play batch leaves the
window frame open, so 'getAssetsMatching' keeps reviving the tombstone that
'RemoveFromPlay' parked -- a frozen copy that still reads as in play.
-}
assetIsGone :: HasCallStack => AssetId -> TestAppT ()
assetIsGone aid = do
  g <- getGame
  (aid `elem` keys (Entities.entitiesAssets (g ^. entitiesL))) `shouldBe` False

-- | Put a real act entity into play the way a scenario's act deck would.
realAct :: CardDef -> TestAppT Act
realAct def = do
  card <- genCard def
  let actId' = ActId (toCardCode card)
      act' = either (error . show) id $ lookupAct actId' 1 (toCardId card)
  overTest $ entitiesL . Entities.actsL %~ insertEntity act'
  pure act'

-- | Exactly one copy: two set asides of the same card would stack the pile.
setAsideHas :: HasCallStack => Card -> TestAppT ()
setAsideHas card =
  scenarioFieldMap ScenarioSetAsideCards (length . filter (== toCardId card) . map toCardId)
    `shouldReturn` 1
