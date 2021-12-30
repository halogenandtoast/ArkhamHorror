module Arkham.Location.Cards.ArkhamWoodsLakeside where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards (arkhamWoodsLakeside)
import Arkham.Classes
import Arkham.Criteria
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype ArkhamWoodsLakeside = ArkhamWoodsLakeside LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsLakeside :: LocationCard ArkhamWoodsLakeside
arkhamWoodsLakeside = locationWithRevealedSideConnections
  ArkhamWoodsLakeside
  Cards.arkhamWoodsLakeside
  2
  (PerPlayer 1)
  Square
  [Squiggle]
  Star
  [Squiggle, Heart]

instance HasAbilities ArkhamWoodsLakeside where
  getAbilities (ArkhamWoodsLakeside attrs) =
    withBaseAbilities attrs $
      [ restrictedAbility
          attrs
          1
          (Here <> DuringSkillTest
            (WhileInvestigating $ LocationWithId $ toId attrs)
          )
        $ ForcedAbility
        $ RevealChaosToken Timing.After You AnyToken
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env ArkhamWoodsLakeside where
  runMessage msg l@(ArkhamWoodsLakeside attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      l <$ push (DrawAnotherToken iid)
    _ -> ArkhamWoodsLakeside <$> runMessage msg attrs
