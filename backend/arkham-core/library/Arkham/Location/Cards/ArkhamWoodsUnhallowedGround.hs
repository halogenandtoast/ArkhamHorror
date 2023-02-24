module Arkham.Location.Cards.ArkhamWoodsUnhallowedGround where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( arkhamWoodsUnhallowedGround )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype ArkhamWoodsUnhallowedGround = ArkhamWoodsUnhallowedGround LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsUnhallowedGround :: LocationCard ArkhamWoodsUnhallowedGround
arkhamWoodsUnhallowedGround = location
  ArkhamWoodsUnhallowedGround
  Cards.arkhamWoodsUnhallowedGround
  4
  (PerPlayer 1)

instance HasAbilities ArkhamWoodsUnhallowedGround where
  getAbilities (ArkhamWoodsUnhallowedGround x) | locationRevealed x =
    withBaseAbilities x
      $ [ mkAbility x 1
          $ ForcedAbility
          $ Enters Timing.After You
          $ LocationWithId
          $ toId x
        ]
  getAbilities (ArkhamWoodsUnhallowedGround x) = getAbilities x

instance RunMessage ArkhamWoodsUnhallowedGround where
  runMessage msg l@(ArkhamWoodsUnhallowedGround attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> l <$ push
      (beginSkillTest
        iid
        source
        (InvestigatorTarget iid)
        SkillWillpower
        4
      )
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> l
      <$ push (InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 1)
    _ -> ArkhamWoodsUnhallowedGround <$> runMessage msg attrs
