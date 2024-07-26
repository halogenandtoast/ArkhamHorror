module Arkham.Asset.Cards.SparrowMaskTheWanderersCompanion (
  sparrowMaskTheWanderersCompanion,
  SparrowMaskTheWanderersCompanion (..),
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

newtype SparrowMaskTheWanderersCompanion = SparrowMaskTheWanderersCompanion AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sparrowMaskTheWanderersCompanion :: AssetCard SparrowMaskTheWanderersCompanion
sparrowMaskTheWanderersCompanion = asset SparrowMaskTheWanderersCompanion Cards.sparrowMaskTheWanderersCompanion

instance HasAbilities SparrowMaskTheWanderersCompanion where
  getAbilities (SparrowMaskTheWanderersCompanion a) =
    [ playerLimit PerTestOrAbility
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
    CheckWindow iids ws | maybe False (`elem` iids) attrs.controller -> do
      when (attrs.use Offering < 2) do
        for_ attrs.controller \iid -> do
          replenish <-
            anyM
              ( \w ->
                  windowMatches iid (toSource attrs) w
                    $ DealtDamageOrHorror #after AnySource You
              )
              ws
          when replenish $ placeTokens attrs attrs Offering 1
      pure a
    _ -> SparrowMaskTheWanderersCompanion <$> liftRunMessage msg attrs
