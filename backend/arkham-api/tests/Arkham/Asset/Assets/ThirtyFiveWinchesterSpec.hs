module Arkham.Asset.Assets.ThirtyFiveWinchesterSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Card.PlayerCard qualified as PlayerCard
import Arkham.Investigator.Cards (rolandBanks)
import Arkham.Matcher
import Arkham.Taboo.Types
import TestImport.New

spec :: Spec
spec = describe ".35 Winchester" $ do
  it "does not deal +2 damage when a Curse token reveals the AutoFail token" . gameTest $ \self -> do
    winchesterCard <-
      genPlayerCardWith Assets.thirtyFiveWinchester
        $ PlayerCard.setTaboo (Just TabooList18)
        . setPlayerCardOwner self.id
    run $ PutCardIntoPlay self.id (toCard winchesterCard) Nothing NoPayment []
    winchester <- selectJust $ assetIs Assets.thirtyFiveWinchester

    teammate <- addInvestigator rolandBanks
    enemy <- testEnemy
    location <- testLocation
    setChaosTokens [CurseToken, AutoFail]
    teammate `moveTo` location
    enemy `spawnAt` location
    self `moveTo` location

    [doFight] <- self `getActionsFrom` winchester
    self `useAbility` doFight
    chooseTarget enemy
    run $ ForceChaosTokenDraw CurseToken
    applyResults
    applyAllDamage

    teammate.damage `shouldReturn` 1
