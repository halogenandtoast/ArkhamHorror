module Arkham.Location.Cards.ArkhamWoodsQuietGlade
  ( ArkhamWoodsQuietGlade(..)
  , arkhamWoodsQuietGlade
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( arkhamWoodsQuietGlade )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Message
import Arkham.Source
import Arkham.Target

newtype ArkhamWoodsQuietGlade = ArkhamWoodsQuietGlade LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsQuietGlade :: LocationCard ArkhamWoodsQuietGlade
arkhamWoodsQuietGlade =
  location ArkhamWoodsQuietGlade Cards.arkhamWoodsQuietGlade 1 (Static 0)

instance HasAbilities ArkhamWoodsQuietGlade where
  getAbilities (ArkhamWoodsQuietGlade attrs) | locationRevealed attrs =
    withBaseAbilities attrs
      $ [ restrictedAbility attrs 1 Here (ActionAbility Nothing $ ActionCost 1)
          & abilityLimitL
          .~ PlayerLimit PerTurn 1
        ]
  getAbilities (ArkhamWoodsQuietGlade attrs) = getAbilities attrs

instance RunMessage ArkhamWoodsQuietGlade where
  runMessage msg l@(ArkhamWoodsQuietGlade attrs@LocationAttrs {..}) =
    case msg of
      UseCardAbility iid (LocationSource lid) 1 _ _ | lid == locationId ->
        l <$ pushAll
          [ HealDamage (InvestigatorTarget iid) 1
          , HealHorror (InvestigatorTarget iid) 1
          ]
      _ -> ArkhamWoodsQuietGlade <$> runMessage msg attrs
