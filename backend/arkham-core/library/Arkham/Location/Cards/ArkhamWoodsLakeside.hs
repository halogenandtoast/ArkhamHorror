module Arkham.Location.Cards.ArkhamWoodsLakeside where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( arkhamWoodsLakeside )
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype ArkhamWoodsLakeside = ArkhamWoodsLakeside LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsLakeside :: LocationCard ArkhamWoodsLakeside
arkhamWoodsLakeside =
  location ArkhamWoodsLakeside Cards.arkhamWoodsLakeside 2 (PerPlayer 1)

instance HasAbilities ArkhamWoodsLakeside where
  getAbilities (ArkhamWoodsLakeside attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility
            attrs
            1
            (Here <> DuringSkillTest
              (WhileInvestigating $ LocationWithId $ toId attrs)
            )
          $ ForcedAbility
          $ RevealChaosToken Timing.After You AnyToken
        | locationRevealed attrs
        ]

instance RunMessage ArkhamWoodsLakeside where
  runMessage msg l@(ArkhamWoodsLakeside attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      l <$ push (DrawAnotherToken iid)
    _ -> ArkhamWoodsLakeside <$> runMessage msg attrs
