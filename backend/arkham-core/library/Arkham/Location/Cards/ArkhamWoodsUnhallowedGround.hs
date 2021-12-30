module Arkham.Location.Cards.ArkhamWoodsUnhallowedGround where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards (arkhamWoodsUnhallowedGround)
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Attrs
import Arkham.Location.Helpers
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype ArkhamWoodsUnhallowedGround = ArkhamWoodsUnhallowedGround LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsUnhallowedGround :: LocationCard ArkhamWoodsUnhallowedGround
arkhamWoodsUnhallowedGround = locationWithRevealedSideConnections
  ArkhamWoodsUnhallowedGround
  Cards.arkhamWoodsUnhallowedGround
  4
  (PerPlayer 1)
  Square
  [Squiggle]
  Triangle
  [Squiggle, Hourglass, Diamond]

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

instance LocationRunner env => RunMessage env ArkhamWoodsUnhallowedGround where
  runMessage msg l@(ArkhamWoodsUnhallowedGround attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ push
      (BeginSkillTest
        iid
        source
        (InvestigatorTarget iid)
        Nothing
        SkillWillpower
        4
      )
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> l
      <$ push (InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 1)
    _ -> ArkhamWoodsUnhallowedGround <$> runMessage msg attrs
