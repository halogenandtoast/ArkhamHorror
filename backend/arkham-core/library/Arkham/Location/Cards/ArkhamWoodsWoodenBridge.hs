module Arkham.Location.Cards.ArkhamWoodsWoodenBridge where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Criteria
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( arkhamWoodsWoodenBridge )
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype ArkhamWoodsWoodenBridge = ArkhamWoodsWoodenBridge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsWoodenBridge :: LocationCard ArkhamWoodsWoodenBridge
arkhamWoodsWoodenBridge =
  location ArkhamWoodsWoodenBridge Cards.arkhamWoodsWoodenBridge 3 (PerPlayer 1)

instance HasAbilities ArkhamWoodsWoodenBridge where
  getAbilities (ArkhamWoodsWoodenBridge attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility
            attrs
            1
            (Here <> DuringSkillTest (WhileEvadingAnEnemy AnyEnemy))
          $ ForcedAbility
          $ RevealChaosToken Timing.When You AnyToken
        | locationRevealed attrs
        ]

instance RunMessage ArkhamWoodsWoodenBridge where
  runMessage msg l@(ArkhamWoodsWoodenBridge attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (DrawAnotherToken iid)
    _ -> ArkhamWoodsWoodenBridge <$> runMessage msg attrs
