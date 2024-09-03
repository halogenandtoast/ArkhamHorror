module Arkham.Asset.Cards.RelicOfAgesRepossessThePast (
  relicOfAgesRepossessThePast,
  RelicOfAgesRepossessThePast (..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude

newtype Metadata = Metadata {successTriggered :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype RelicOfAgesRepossessThePast = RelicOfAgesRepossessThePast (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

relicOfAgesRepossessThePast :: AssetCard RelicOfAgesRepossessThePast
relicOfAgesRepossessThePast =
  asset
    (RelicOfAgesRepossessThePast . (`with` Metadata False))
    Cards.relicOfAgesRepossessThePast

instance HasAbilities RelicOfAgesRepossessThePast where
  getAbilities (RelicOfAgesRepossessThePast (a `With` _)) =
    [skillTestAbility $ restrictedAbility a 1 ControlsThis $ FastAbility $ exhaust a]

instance RunMessage RelicOfAgesRepossessThePast where
  runMessage msg a@(RelicOfAgesRepossessThePast (attrs `With` metadata)) =
    case msg of
      UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
        sid <- getRandom
        let
          chooseSkillTest skillType = beginSkillTest sid iid (attrs.ability 1) iid skillType (Fixed 4)
        player <- getPlayer iid
        push
          $ chooseOne
            player
            [ SkillLabel skillType [chooseSkillTest skillType]
            | skillType <- [#willpower, #intellect]
            ]
        pure a
      PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) | not (successTriggered metadata) -> do
        targets <- targetsWithDoom
        player <- getPlayer iid
        push
          $ chooseOne
            player
            [targetLabel target [RemoveDoom (toAbilitySource attrs 1) target 1] | target <- targets]
        pure . RelicOfAgesRepossessThePast $ attrs `with` Metadata True
      _ -> RelicOfAgesRepossessThePast . (`with` metadata) <$> runMessage msg attrs
