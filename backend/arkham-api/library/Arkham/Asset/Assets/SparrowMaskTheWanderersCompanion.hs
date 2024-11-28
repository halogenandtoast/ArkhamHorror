module Arkham.Asset.Assets.SparrowMaskTheWanderersCompanion (
  sparrowMaskTheWanderersCompanion,
  SparrowMaskTheWanderersCompanion (..),
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

newtype SparrowMaskTheWanderersCompanion = SparrowMaskTheWanderersCompanion AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sparrowMaskTheWanderersCompanion :: AssetCard SparrowMaskTheWanderersCompanion
sparrowMaskTheWanderersCompanion = asset SparrowMaskTheWanderersCompanion Cards.sparrowMaskTheWanderersCompanion

instance HasAbilities SparrowMaskTheWanderersCompanion where
  getAbilities (SparrowMaskTheWanderersCompanion a) =
    [ playerLimit PerTestOrAbility
        $ wantsSkillTest (YourSkillTest $ oneOf [#willpower, #agility])
        $ controlledAbility a 1 DuringAnySkillTest
        $ FastAbility
        $ assetUseCost a Offering 1
    ]

instance RunMessage SparrowMaskTheWanderersCompanion where
  runMessage msg a@(SparrowMaskTheWanderersCompanion attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        chooseOneM iid do
          labeled "Get +2 {willpower}"
            $ skillTestModifier sid (attrs.ability 1) iid (SkillModifier #willpower 2)
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
                      $ oneOf
                        [ DealtDamageOrHorror #after AnySource You
                        , DealtDamage #after AnySource You
                        , DealtHorror #after AnySource You
                        ]
                )
                ws
            depth <- getWindowDepth
            let alreadyReplenished = isJust $ getAssetMetaDefault @(Maybe Int) Nothing attrs
            if shouldReplenish && not alreadyReplenished
              then do
                placeTokens attrs attrs Offering 1
                pure $ SparrowMaskTheWanderersCompanion $ attrs & setMeta (Just depth)
              else pure a
        else pure a
    EndCheckWindow -> do
      depth <- getWindowDepth
      pure $ case getAssetMetaDefault @(Maybe Int) Nothing attrs of
        Just d | depth < d -> SparrowMaskTheWanderersCompanion $ attrs & setMeta @(Maybe Int) Nothing
        _ -> a
    _ -> SparrowMaskTheWanderersCompanion <$> liftRunMessage msg attrs
