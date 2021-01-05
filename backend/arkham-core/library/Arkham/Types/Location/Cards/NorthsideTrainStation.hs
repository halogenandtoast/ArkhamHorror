module Arkham.Types.Location.Cards.NorthsideTrainStation
  ( NorthsideTrainStation(..)
  , northsideTrainStation
  )
where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype NorthsideTrainStation = NorthsideTrainStation Attrs
  deriving newtype (Show, ToJSON, FromJSON)

northsideTrainStation :: NorthsideTrainStation
northsideTrainStation = NorthsideTrainStation
  (baseAttrs
    "50028"
    (LocationName "Northside" $ Just "Train Station")
    EncounterSet.TheMidnightMasks
    2
    (PerPlayer 1)
    T
    [Diamond, Triangle]
    [Arkham]
  )

instance HasModifiersFor env NorthsideTrainStation where
  getModifiersFor = noModifiersFor

ability :: Attrs -> Ability
ability attrs =
  (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1))
    { abilityLimit = PlayerLimit PerGame 1
    }

instance ActionRunner env => HasActions env NorthsideTrainStation where
  getActions iid NonFast (NorthsideTrainStation attrs@Attrs {..})
    | locationRevealed = withBaseActions iid NonFast attrs $ pure
      [ ActivateCardAbilityAction iid (ability attrs)
      | iid `member` locationInvestigators
      ]
  getActions iid window (NorthsideTrainStation attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env NorthsideTrainStation where
  runMessage msg l@(NorthsideTrainStation attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      locationIds <- getSetList [Arkham]
      l <$ unshiftMessage
        (chooseOne iid [ MoveTo iid lid | lid <- locationIds ])
    _ -> NorthsideTrainStation <$> runMessage msg attrs
