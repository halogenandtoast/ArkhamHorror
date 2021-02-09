module Arkham.Types.Location.Cards.ArkhamWoodsQuietGlade
  ( ArkhamWoodsQuietGlade(..)
  , arkhamWoodsQuietGlade
  ) where


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype ArkhamWoodsQuietGlade = ArkhamWoodsQuietGlade LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsQuietGlade :: ArkhamWoodsQuietGlade
arkhamWoodsQuietGlade = ArkhamWoodsQuietGlade $ base
  { locationRevealedConnectedSymbols = setFromList [Squiggle, Equals, Hourglass]
  , locationRevealedSymbol = Moon
  }
 where
  base = baseAttrs
    "01155"
    (Name "Arkham Woods" (Just "Quiet Glade"))
    EncounterSet.TheDevourerBelow
    1
    (Static 0)
    Square
    [Squiggle]
    [Woods]

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
  runMessage msg l@(ArkhamWoodsQuietGlade attrs@LocationAttrs {..}) = case msg of
    UseCardAbility iid (LocationSource lid) _ 1 _ | lid == locationId ->
      l <$ unshiftMessages
        [ HealDamage (InvestigatorTarget iid) 1
        , HealHorror (InvestigatorTarget iid) 1
        ]
    _ -> ArkhamWoodsQuietGlade <$> runMessage msg attrs
