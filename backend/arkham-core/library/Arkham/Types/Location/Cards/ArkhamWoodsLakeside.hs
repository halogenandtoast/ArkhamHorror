module Arkham.Types.Location.Cards.ArkhamWoodsLakeside where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (arkhamWoodsLakeside)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import qualified Arkham.Types.Timing as Timing

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

instance HasAbilities env ArkhamWoodsLakeside where
  getAbilities i window (ArkhamWoodsLakeside attrs) =
    withBaseAbilities i window attrs $ pure
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
