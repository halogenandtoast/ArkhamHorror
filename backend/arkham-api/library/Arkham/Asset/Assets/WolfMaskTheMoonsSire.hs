module Arkham.Asset.Assets.WolfMaskTheMoonsSire (wolfMaskTheMoonsSire, WolfMaskTheMoonsSire (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Game.Helpers (windowMatches)
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype WolfMaskTheMoonsSire = WolfMaskTheMoonsSire AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wolfMaskTheMoonsSire :: AssetCard WolfMaskTheMoonsSire
wolfMaskTheMoonsSire = asset WolfMaskTheMoonsSire Cards.wolfMaskTheMoonsSire

instance HasAbilities WolfMaskTheMoonsSire where
  getAbilities (WolfMaskTheMoonsSire a) =
    [ playerLimit PerTestOrAbility
        $ wantsSkillTest (YourSkillTest $ oneOf [#combat, #agility])
        $ controlledAbility a 1 DuringAnySkillTest
        $ FastAbility
        $ assetUseCost a Offering 1
    ]

instance RunMessage WolfMaskTheMoonsSire where
  runMessage msg a@(WolfMaskTheMoonsSire attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        chooseOneM iid do
          labeled "Get +2 {combat}"
            $ skillTestModifier sid (attrs.ability 1) iid (SkillModifier #combat 2)
          labeled "Get +2 {agility}" $ skillTestModifier sid (attrs.ability 1) iid (SkillModifier #agility 2)
      pure a
    Do (CheckWindows ws) -> do
      if attrs.use Offering < 2
        then case attrs.controller of
          Nothing -> pure a
          Just iid -> do
            shouldReplenish <-
              anyM
                ( \w ->
                    windowMatches iid (toSource attrs) w
                      $ EnemyEngaged #after You AnyEnemy
                )
                ws
            depth <- getWindowDepth
            let alreadyReplenished = isJust $ getAssetMetaDefault @(Maybe Int) Nothing attrs
            if shouldReplenish && not alreadyReplenished
              then do
                placeTokens attrs attrs Offering 1
                pure $ WolfMaskTheMoonsSire $ attrs & setMeta (Just depth)
              else pure a
        else pure a
    EndCheckWindow -> do
      depth <- getWindowDepth
      pure $ case getAssetMetaDefault @(Maybe Int) Nothing attrs of
        Just d | depth < d -> WolfMaskTheMoonsSire $ attrs & setMeta @(Maybe Int) Nothing
        _ -> a
    _ -> WolfMaskTheMoonsSire <$> liftRunMessage msg attrs
