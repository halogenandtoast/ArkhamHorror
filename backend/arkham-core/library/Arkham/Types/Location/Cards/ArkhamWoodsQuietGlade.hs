module Arkham.Types.Location.Cards.ArkhamWoodsQuietGlade
  ( ArkhamWoodsQuietGlade(..)
  , arkhamWoodsQuietGlade
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (arkhamWoodsQuietGlade)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Window

newtype ArkhamWoodsQuietGlade = ArkhamWoodsQuietGlade LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsQuietGlade :: LocationId -> ArkhamWoodsQuietGlade
arkhamWoodsQuietGlade =
  ArkhamWoodsQuietGlade
    . (revealedConnectedSymbolsL .~ setFromList [Squiggle, Equals, Hourglass])
    . (revealedSymbolL .~ Moon)
    . baseAttrs
        Cards.arkhamWoodsQuietGlade
        1
        (Static 0)
        Square
        [Squiggle]

instance HasModifiersFor env ArkhamWoodsQuietGlade where
  getModifiersFor = noModifiersFor

ability :: LocationAttrs -> Ability
ability attrs =
  (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1))
    { abilityLimit = PlayerLimit PerTurn 1
    }

instance ActionRunner env => HasActions env ArkhamWoodsQuietGlade where
  getActions iid NonFast (ArkhamWoodsQuietGlade attrs@LocationAttrs {..})
    | locationRevealed = withBaseActions iid NonFast attrs $ pure
      [ ActivateCardAbilityAction iid (ability attrs)
      | iid `elem` locationInvestigators
      ]
  getActions iid window (ArkhamWoodsQuietGlade attrs) =
    getActions iid window attrs

instance (LocationRunner env) => RunMessage env ArkhamWoodsQuietGlade where
  runMessage msg l@(ArkhamWoodsQuietGlade attrs@LocationAttrs {..}) =
    case msg of
      UseCardAbility iid (LocationSource lid) _ 1 _ | lid == locationId ->
        l <$ unshiftMessages
          [ HealDamage (InvestigatorTarget iid) 1
          , HealHorror (InvestigatorTarget iid) 1
          ]
      _ -> ArkhamWoodsQuietGlade <$> runMessage msg attrs
