module Arkham.Debug.SealSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.ChaosBag.Base
import Arkham.Helpers.Scenario
import Arkham.Projection (field, getAttrs)
import Arkham.Scenario.Types (Field (..))
import Arkham.Treachery.CardDefs.EdgeOfTheEarth qualified as Treacheries
import Arkham.Treachery.Types (Treachery, TreacheryAttrs (..))
import TestImport.New

bagTokens :: TestAppT [ChaosToken]
bagTokens = scenarioFieldMap ScenarioChaosBag chaosBagChaosTokens

treacherySeals :: TreacheryId -> TestAppT [ChaosToken]
treacherySeals tid = treacherySealedChaosTokens <$> getAttrs @Treachery tid

spec :: Spec
spec = describe "Debug sealing" do
  it "seals a bag token on a treachery" . gameTest $ \self -> do
    tid <- self `putTreacheryIntoPlay` Treacheries.legInjury
    setChaosTokens [Zero]
    [token] <- bagTokens
    run $ DebugSealChaosToken (chaosTokenId token) (toTarget tid)
    treacherySeals tid `shouldReturn` [token]
    bagTokens `shouldReturn` []

  it "a token sealed elsewhere leaves the treachery" . gameTest $ \self -> do
    tid <- self `putTreacheryIntoPlay` Treacheries.legInjury
    aid <- self `putAssetIntoPlay` Assets.flashlight
    setChaosTokens [Zero]
    [token] <- bagTokens
    run $ DebugSealChaosToken (chaosTokenId token) (toTarget tid)
    run $ SealedChaosToken token Nothing (toTarget aid)
    treacherySeals tid `shouldReturn` []
    (map chaosTokenId <$> field AssetSealedChaosTokens aid) `shouldReturn` [chaosTokenId token]

  it "sealing the same token twice keeps one copy" . gameTest $ \self -> do
    tid <- self `putTreacheryIntoPlay` Treacheries.legInjury
    setChaosTokens [Zero]
    [token] <- bagTokens
    run $ SealedChaosToken token Nothing (toTarget tid)
    run $ SealedChaosToken token Nothing (toTarget tid)
    treacherySeals tid `shouldReturn` [token]

  it "releasing to the pool takes a bless token out of play" . gameTest $ \self -> do
    tid <- self `putTreacheryIntoPlay` Treacheries.legInjury
    setChaosTokens [BlessToken]
    [token] <- bagTokens
    run $ DebugSealChaosToken (chaosTokenId token) (toTarget tid)
    run $ Run [UnsealChaosToken token, ReturnChaosTokensToPool [token]]
    treacherySeals tid `shouldReturn` []
    bagTokens `shouldReturn` []
