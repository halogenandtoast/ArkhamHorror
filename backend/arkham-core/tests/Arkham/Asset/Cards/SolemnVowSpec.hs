module Arkham.Asset.Cards.SolemnVowSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards (rolandBanks)
import TestImport.New

data MoveType = MoveDamage | MoveHorror
  deriving stock Eq

instance Show MoveType where
  show MoveDamage = "damage"
  show MoveHorror = "horror"

data SourceType = You | AssetYouControl
  deriving stock Eq

instance Show SourceType where
  show You = "you"
  show AssetYouControl = "an asset you control"

data TargetType = Owner | AssetOwnerControls
  deriving stock Eq

instance Show TargetType where
  show Owner = "the owner"
  show AssetOwnerControls = "an asset the owner controls"

testPermutations :: [(MoveType, SourceType, TargetType)]
testPermutations = [(dType, sType, tType) | dType <- [MoveDamage, MoveHorror], sType <- [You, AssetYouControl], tType <- [Owner, AssetOwnerControls]]

spec :: Spec
spec = describe "Solemn Vow" do
  it "play under the control of another investigator at your location" . gameTest $ \self -> do
    roland <- addInvestigator rolandBanks
    location <- testLocation
    self `moveTo` location
    roland `moveTo` location
    solemnVow <- self `putAssetIntoPlay` Assets.solemnVow

    solemnVow.controller `shouldReturn` Just (toId roland)
    solemnVow.owner `shouldReturn` Just (toId self)

  context "if the owner of Solemn Vow is at your location" do
    context "exhaust solemn vow, move 1 damage or horror from a card you control to a card that investigator controls" do
      for_ testPermutations $ \(dType, sType, tType) -> do
        it ("can move a " <> show dType <> " from " <> show sType <>  " to " <> show tType) . gameTest $ \self -> do
          leoDeLuca <- self `putAssetIntoPlay` Assets.leoDeLuca
          roland <- addInvestigator rolandBanks & prop @"damage" 1 & prop @"horror" 1
          beatCop2 <- roland `putAssetIntoPlay` Assets.beatCop2
          run $ AssetDamage beatCop2 (TestSource mempty) 1 1
          location <- testLocation
          self `moveTo` location
          roland `moveTo` location
          solemnVow <- self `putAssetIntoPlay` Assets.solemnVow
          setActive roland
          [useSolemnVow] <- roland `getActionsFrom` solemnVow
          roland `useAbility` useSolemnVow

          let
            fromToken = if dType == MoveDamage then DamageToken else HorrorToken
            fromMatcher =
              case sType of
                You -> \case
                  ComponentLabel (InvestigatorComponent _ tok) _ -> tok == fromToken
                  _ -> False
                AssetYouControl -> \case
                  ComponentLabel (AssetComponent _ tok) _ -> tok == fromToken
                  _ -> False
            
          chooseOptionMatching "Choose source" fromMatcher

          case tType of
            Owner -> chooseTarget self
            AssetOwnerControls -> chooseTarget leoDeLuca
      
          roland.damage `shouldReturn` (if (dType, sType) == (MoveDamage, You) then 0 else 1)
          roland.horror `shouldReturn` (if (dType, sType) == (MoveHorror, You) then 0 else 1)
          beatCop2.damage `shouldReturn` (if (dType, sType) == (MoveDamage, AssetYouControl) then 0 else 1)
          beatCop2.horror `shouldReturn` (if (dType, sType) == (MoveHorror, AssetYouControl) then 0 else 1)

          self.damage `shouldReturn` (if (dType, tType) == (MoveDamage, Owner) then 1 else 0)
          self.horror `shouldReturn` (if (dType, tType) == (MoveHorror, Owner) then 1 else 0)
          leoDeLuca.damage `shouldReturn` (if (dType, tType) == (MoveDamage, AssetOwnerControls) then 1 else 0)
          leoDeLuca.horror `shouldReturn` (if (dType, tType) == (MoveHorror, AssetOwnerControls) then 1 else 0)

  context "if the owner of Solemn Vow is not at your location" do
    it "can't be used" . gameTest $ \self -> do
      roland <- addInvestigator rolandBanks & prop @"damage" 1
      (location1, location2) <- testConnectedLocations id id
      self `moveTo` location1
      roland `moveTo` location1
      solemnVow <- self `putAssetIntoPlay` Assets.solemnVow
      self `moveTo` location2
      setActive roland
      actions <- roland `getActionsFrom` solemnVow
      actions `shouldBe` []
