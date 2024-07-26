module Arkham.Asset.Cards.FoxMaskTheWiseTrickster (
  foxMaskTheWiseTrickster,
  FoxMaskTheWiseTrickster (..),
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

newtype FoxMaskTheWiseTrickster = FoxMaskTheWiseTrickster AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foxMaskTheWiseTrickster :: AssetCard FoxMaskTheWiseTrickster
foxMaskTheWiseTrickster = asset FoxMaskTheWiseTrickster Cards.foxMaskTheWiseTrickster

instance HasAbilities FoxMaskTheWiseTrickster where
  getAbilities (FoxMaskTheWiseTrickster a) =
    [ playerLimit PerTestOrAbility
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
    CheckWindow iids ws | maybe False (`elem` iids) attrs.controller -> do
      when (attrs.use Offering < 2) do
        for_ attrs.controller \iid -> do
          replenish <-
            anyM
              ( \w ->
                  windowMatches iid (toSource attrs) w
                    $ Moves #after You AnySource (LocationWithEnemy AnyEnemy) Anywhere
              )
              ws
          when replenish $ placeTokens attrs attrs Offering 1
      pure a
    _ -> FoxMaskTheWiseTrickster <$> liftRunMessage msg attrs
