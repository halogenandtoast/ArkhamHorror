module Arkham.Asset.Cards.CatMaskTheCapriciousMeddler (
  catMaskTheCapriciousMeddler,
  CatMaskTheCapriciousMeddler (..),
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

newtype CatMaskTheCapriciousMeddler = CatMaskTheCapriciousMeddler AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

catMaskTheCapriciousMeddler :: AssetCard CatMaskTheCapriciousMeddler
catMaskTheCapriciousMeddler = asset CatMaskTheCapriciousMeddler Cards.catMaskTheCapriciousMeddler

instance HasAbilities CatMaskTheCapriciousMeddler where
  getAbilities (CatMaskTheCapriciousMeddler a) =
    [ playerLimit PerTestOrAbility
        $ controlledAbility a 1 DuringAnySkillTest
        $ FastAbility
        $ assetUseCost a Offering 1
    ]

instance RunMessage CatMaskTheCapriciousMeddler where
  runMessage msg a@(CatMaskTheCapriciousMeddler attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        chooseOneM iid do
          labeled "Get +2 {willpower}"
            $ skillTestModifier sid (attrs.ability 1) iid (SkillModifier #willpower 2)
          labeled "Get +2 {combat}" $ skillTestModifier sid (attrs.ability 1) iid (SkillModifier #combat 2)
      pure a
    CheckWindow iids ws | maybe False (`elem` iids) attrs.controller -> do
      when (attrs.use Offering < 2) do
        for_ attrs.controller \iid -> do
          replenish <-
            anyM
              ( \w ->
                  windowMatches iid (toSource attrs) w
                    $ PlacedDoomCounterOnTargetWithNoDoom #after AnySource AnyTarget
              )
              ws
          when replenish $ placeTokens attrs attrs Offering (2 - attrs.use Offering)
      pure a
    _ -> CatMaskTheCapriciousMeddler <$> liftRunMessage msg attrs
