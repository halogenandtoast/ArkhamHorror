module Arkham.Asset.Cards.HolyRosarySpec ( spec)
where

import TestImport.New
import Arkham.Asset.Cards qualified as Assets
 
spec :: Spec
spec = describe "Holy Rosary" $ do
  gives @"willpower" Assets.holyRosary 1
