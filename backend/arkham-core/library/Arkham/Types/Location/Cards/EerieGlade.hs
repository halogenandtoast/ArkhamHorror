module Arkham.Types.Location.Cards.EerieGlade
  ( eerieGlade
  , EerieGlade(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Classes
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Trait
import Arkham.Types.Window

newtype EerieGlade = EerieGlade LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eerieGlade :: LocationId -> EerieGlade
eerieGlade =
  EerieGlade
    . (revealedSymbolL .~ Hourglass)
    . (revealedConnectedSymbolsL .~ setFromList [Triangle, Plus])
    . (unrevealedNameL .~ "Diverging Path")
    . baseAttrs
        "02286"
        "Eerie Glade"
        EncounterSet.WhereDoomAwaits
        4
        (PerPlayer 1)
        NoSymbol
        []
        [Dunwich, Woods]

instance HasModifiersFor env EerieGlade where
  getModifiersFor = noModifiersFor

forcedAbility :: LocationAttrs -> Ability
forcedAbility a = mkAbility (toSource a) 1 ForcedAbility

instance ActionRunner env => HasActions env EerieGlade where
  getActions iid (AfterRevealLocation You) (EerieGlade attrs) | iid `on` attrs =
    do
      actionRemainingCount <- unActionRemainingCount <$> getCount iid
      pure
        [ ActivateCardAbilityAction iid (forcedAbility attrs)
        | actionRemainingCount > 0
        ]
  getActions iid window (EerieGlade attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env EerieGlade where
  runMessage msg l@(EerieGlade attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      actionRemainingCount <- unActionRemainingCount <$> getCount iid
      l <$ unshiftMessage
        (DiscardTopOfDeck iid (actionRemainingCount * 2) Nothing)
    _ -> EerieGlade <$> runMessage msg attrs
