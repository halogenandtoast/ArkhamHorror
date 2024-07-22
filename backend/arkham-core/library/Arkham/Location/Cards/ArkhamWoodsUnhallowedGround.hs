module Arkham.Location.Cards.ArkhamWoodsUnhallowedGround where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (arkhamWoodsUnhallowedGround)
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype ArkhamWoodsUnhallowedGround = ArkhamWoodsUnhallowedGround LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsUnhallowedGround :: LocationCard ArkhamWoodsUnhallowedGround
arkhamWoodsUnhallowedGround =
  location
    ArkhamWoodsUnhallowedGround
    Cards.arkhamWoodsUnhallowedGround
    4
    (PerPlayer 1)

instance HasAbilities ArkhamWoodsUnhallowedGround where
  getAbilities (ArkhamWoodsUnhallowedGround x) =
    withRevealedAbilities x
      $ [ forcedAbility x 1
            $ Enters Timing.After You
            $ LocationWithId (toId x)
        ]

instance RunMessage ArkhamWoodsUnhallowedGround where
  runMessage msg l@(ArkhamWoodsUnhallowedGround attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      sid <- getRandom
      push $ beginSkillTest sid iid (toAbilitySource attrs 1) iid #willpower (Fixed 4)
      pure l
    FailedSkillTest iid _ (isAbilitySource attrs 1 -> True) SkillTestInitiatorTarget {} _ _ -> do
      push $ assignDamageAndHorror iid attrs 1 1
      pure l
    _ -> ArkhamWoodsUnhallowedGround <$> runMessage msg attrs
