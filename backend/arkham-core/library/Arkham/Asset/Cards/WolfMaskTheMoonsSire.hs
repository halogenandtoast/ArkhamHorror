module Arkham.Asset.Cards.WolfMaskTheMoonsSire (
  wolfMaskTheMoonsSire,
  WolfMaskTheMoonsSire (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Game.Helpers (windowMatches)
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
    CheckWindow iids ws | maybe False (`elem` iids) attrs.controller -> do
      when (attrs.use Offering < 2) do
        for_ attrs.controller \iid -> do
          replenish <-
            anyM
              ( \w ->
                  windowMatches iid (toSource attrs) w
                    $ EnemyEngaged #after You AnyEnemy
              )
              ws
          when replenish $ placeTokens attrs attrs Offering 1
      pure a
    _ -> WolfMaskTheMoonsSire <$> liftRunMessage msg attrs
