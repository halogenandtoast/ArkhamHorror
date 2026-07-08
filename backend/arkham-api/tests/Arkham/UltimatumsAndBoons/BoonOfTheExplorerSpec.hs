module Arkham.UltimatumsAndBoons.BoonOfTheExplorerSpec (spec) where

import Arkham.Action qualified as Action
import Arkham.Action.Additional
import Arkham.Helpers.Action (getAdditionalActions)
import Helpers.UltimatumsAndBoons
import TestImport.New

spec :: Spec
spec = describe "Boon of the Explorer" $ do
  it "grants an additional action that can only be used to explore" . gameTest $ \self -> do
    withUltimatumsAndBoons [BoonOfTheExplorer]
    attrs <- toAttrs <$> getInvestigator (toId self)
    getAdditionalActions attrs
      `shouldContainM` [ AdditionalAction
                           "Boon of the Explorer"
                           (UltimatumOrBoonSource (Boon BoonOfTheExplorer))
                           (ActionRestrictedAdditionalAction Action.Explore)
                       ]
