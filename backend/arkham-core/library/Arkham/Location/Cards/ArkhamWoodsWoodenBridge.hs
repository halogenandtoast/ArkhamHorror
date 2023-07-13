module Arkham.Location.Cards.ArkhamWoodsWoodenBridge where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (arkhamWoodsWoodenBridge)
import Arkham.Location.Runner hiding (RevealChaosToken)
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype ArkhamWoodsWoodenBridge = ArkhamWoodsWoodenBridge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsWoodenBridge :: LocationCard ArkhamWoodsWoodenBridge
arkhamWoodsWoodenBridge = location ArkhamWoodsWoodenBridge Cards.arkhamWoodsWoodenBridge 3 (PerPlayer 1)

instance HasAbilities ArkhamWoodsWoodenBridge where
  getAbilities (ArkhamWoodsWoodenBridge attrs) =
    withRevealedAbilities attrs $
      [ restrictedAbility
          attrs
          1
          (Here <> DuringSkillTest (WhileEvadingAnEnemy AnyEnemy))
          $ ForcedAbility
          $ RevealChaosToken Timing.When You AnyChaosToken
      ]

instance RunMessage ArkhamWoodsWoodenBridge where
  runMessage msg l@(ArkhamWoodsWoodenBridge attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push (DrawAnotherChaosToken iid)
      pure l
    _ -> ArkhamWoodsWoodenBridge <$> runMessage msg attrs
