module Arkham.Asset.Assets.FoxMaskTheWiseTrickster (
  foxMaskTheWiseTrickster,
  FoxMaskTheWiseTrickster (..),
)
where

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

newtype FoxMaskTheWiseTrickster = FoxMaskTheWiseTrickster AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foxMaskTheWiseTrickster :: AssetCard FoxMaskTheWiseTrickster
foxMaskTheWiseTrickster = asset FoxMaskTheWiseTrickster Cards.foxMaskTheWiseTrickster

instance HasAbilities FoxMaskTheWiseTrickster where
  getAbilities (FoxMaskTheWiseTrickster a) =
    [ playerLimit PerTestOrAbility
        $ wantsSkillTest (YourSkillTest $ oneOf [#intellect, #agility])
        $ controlledAbility a 1 DuringAnySkillTest
        $ FastAbility
        $ assetUseCost a Offering 1
    ]

instance RunMessage FoxMaskTheWiseTrickster where
  runMessage msg a@(FoxMaskTheWiseTrickster attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        chooseOneM iid do
          labeled "Get +2 {intellect}"
            $ skillTestModifier sid (attrs.ability 1) iid (SkillModifier #intellect 2)
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
                      $ Moves #after You AnySource (LocationWithEnemy AnyEnemy) Anywhere
                )
                ws
            depth <- getWindowDepth
            let alreadyReplenished = isJust $ getAssetMetaDefault @(Maybe Int) Nothing attrs
            if shouldReplenish && not alreadyReplenished
              then do
                placeTokens attrs attrs Offering 1
                pure $ FoxMaskTheWiseTrickster $ attrs & setMeta (Just depth)
              else pure a
        else pure a
    EndCheckWindow -> do
      depth <- getWindowDepth
      pure $ case getAssetMetaDefault @(Maybe Int) Nothing attrs of
        Just d | depth < d -> FoxMaskTheWiseTrickster $ attrs & setMeta @(Maybe Int) Nothing
        _ -> a
    _ -> FoxMaskTheWiseTrickster <$> liftRunMessage msg attrs
