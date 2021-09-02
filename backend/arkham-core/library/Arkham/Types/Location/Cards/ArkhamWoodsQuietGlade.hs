module Arkham.Types.Location.Cards.ArkhamWoodsQuietGlade
  ( ArkhamWoodsQuietGlade(..)
  , arkhamWoodsQuietGlade
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (arkhamWoodsQuietGlade)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target

newtype ArkhamWoodsQuietGlade = ArkhamWoodsQuietGlade LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsQuietGlade :: LocationCard ArkhamWoodsQuietGlade
arkhamWoodsQuietGlade = locationWith
  ArkhamWoodsQuietGlade
  Cards.arkhamWoodsQuietGlade
  1
  (Static 0)
  Square
  [Squiggle]
  ((revealedConnectedSymbolsL .~ setFromList [Squiggle, Equals, Hourglass])
  . (revealedSymbolL .~ Moon)
  )

instance HasAbilities ArkhamWoodsQuietGlade where
  getAbilities (ArkhamWoodsQuietGlade attrs)
    | locationRevealed attrs = withBaseAbilities attrs $
      [ restrictedAbility attrs 1 Here (ActionAbility Nothing $ ActionCost 1)
        & abilityLimitL
        .~ PlayerLimit PerTurn 1
      ]
  getAbilities (ArkhamWoodsQuietGlade attrs) =
    getAbilities attrs

instance LocationRunner env => RunMessage env ArkhamWoodsQuietGlade where
  runMessage msg l@(ArkhamWoodsQuietGlade attrs@LocationAttrs {..}) =
    case msg of
      UseCardAbility iid (LocationSource lid) _ 1 _ | lid == locationId ->
        l <$ pushAll
          [ HealDamage (InvestigatorTarget iid) 1
          , HealHorror (InvestigatorTarget iid) 1
          ]
      _ -> ArkhamWoodsQuietGlade <$> runMessage msg attrs
