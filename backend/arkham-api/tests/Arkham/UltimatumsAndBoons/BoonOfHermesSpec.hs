module Arkham.UltimatumsAndBoons.BoonOfHermesSpec (spec) where

import Arkham.Action qualified as Action
import Arkham.Action.Additional
import Arkham.Helpers.Action (getAdditionalActions)
import Helpers.UltimatumsAndBoons
import TestImport.New

spec :: Spec
spec = describe "Boon of Hermes" $ do
  it "grants an additional action that can only be used to move" . gameTest $ \self -> do
    withUltimatumsAndBoons [BoonOfHermes]
    attrs <- toAttrs <$> getInvestigator (toId self)
    getAdditionalActions attrs
      `shouldContainM` [ AdditionalAction
                           "Boon of Hermes"
                           (UltimatumOrBoonSource (Boon BoonOfHermes))
                           (ActionRestrictedAdditionalAction Action.Move)
                       ]

  it "grants nothing when not selected" . gameTest $ \self -> do
    attrs <- toAttrs <$> getInvestigator (toId self)
    getAdditionalActions attrs `shouldReturn` []
