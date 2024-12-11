module Arkham.Asset.Assets.MouseMaskTheMeekWatcher (
  mouseMaskTheMeekWatcher,
  MouseMaskTheMeekWatcher (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealLocation)
import Arkham.Asset.Uses
import Arkham.Game.Helpers (windowMatches)
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype MouseMaskTheMeekWatcher = MouseMaskTheMeekWatcher AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mouseMaskTheMeekWatcher :: AssetCard MouseMaskTheMeekWatcher
mouseMaskTheMeekWatcher = asset MouseMaskTheMeekWatcher Cards.mouseMaskTheMeekWatcher

instance HasAbilities MouseMaskTheMeekWatcher where
  getAbilities (MouseMaskTheMeekWatcher a) =
    [ playerLimit PerTestOrAbility
        $ wantsSkillTest (YourSkillTest $ oneOf [#willpower, #intellect])
        $ controlledAbility a 1 DuringAnySkillTest
        $ FastAbility
        $ assetUseCost a Offering 1
    ]

instance RunMessage MouseMaskTheMeekWatcher where
  runMessage msg a@(MouseMaskTheMeekWatcher attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        chooseOneM iid do
          labeled "Get +2 {willpower}"
            $ skillTestModifier sid (attrs.ability 1) iid (SkillModifier #willpower 2)
          labeled "Get +2 {intellect}"
            $ skillTestModifier sid (attrs.ability 1) iid (SkillModifier #intellect 2)
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
                      $ oneOf
                        [ RevealLocation #after You Anywhere
                        , PutLocationIntoPlay #after You Anywhere
                        ]
                )
                ws
            depth <- getWindowDepth
            let alreadyReplenished = isJust $ getAssetMetaDefault @(Maybe Int) Nothing attrs
            if shouldReplenish && not alreadyReplenished
              then do
                placeTokens attrs attrs Offering 1
                pure $ MouseMaskTheMeekWatcher $ attrs & setMeta (Just depth)
              else pure a
        else pure a
    EndCheckWindow -> do
      depth <- getWindowDepth
      pure $ case getAssetMetaDefault @(Maybe Int) Nothing attrs of
        Just d | depth < d -> MouseMaskTheMeekWatcher $ attrs & setMeta @(Maybe Int) Nothing
        _ -> a
    _ -> MouseMaskTheMeekWatcher <$> liftRunMessage msg attrs
