module Arkham.Types.Location.Cards.ArkhamWoodsWoodenBridge where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (arkhamWoodsWoodenBridge)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import qualified Arkham.Types.Timing as Timing

newtype ArkhamWoodsWoodenBridge = ArkhamWoodsWoodenBridge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsWoodenBridge :: LocationCard ArkhamWoodsWoodenBridge
arkhamWoodsWoodenBridge = locationWith
  ArkhamWoodsWoodenBridge
  Cards.arkhamWoodsWoodenBridge
  3
  (PerPlayer 1)
  Square
  [Squiggle]
  ((revealedConnectedSymbolsL .~ setFromList [Squiggle, Droplet])
  . (revealedSymbolL .~ Circle)
  )

instance HasAbilities env ArkhamWoodsWoodenBridge where
  getAbilities i window (ArkhamWoodsWoodenBridge attrs) =
    withBaseAbilities i window attrs $ pure
      [ restrictedAbility
          attrs
          1
          (Here <> DuringSkillTest (WhileEvadingAnEnemy AnyEnemy))
        $ ForcedAbility
        $ RevealChaosToken Timing.When You AnyToken
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env ArkhamWoodsWoodenBridge where
  runMessage msg l@(ArkhamWoodsWoodenBridge attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (DrawAnotherToken iid)
    _ -> ArkhamWoodsWoodenBridge <$> runMessage msg attrs
