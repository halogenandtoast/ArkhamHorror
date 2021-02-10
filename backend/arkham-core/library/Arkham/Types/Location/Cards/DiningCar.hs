module Arkham.Types.Location.Cards.DiningCar
  ( diningCar
  , DiningCar(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.Card.EncounterCardMatcher
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype DiningCar = DiningCar LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

diningCar :: DiningCar
diningCar = DiningCar
  $ base { locationConnectsTo = setFromList [LeftOf, RightOf] }
 where
  base = baseAttrs
    "02173"
    (Name "Dining Car" Nothing)
    EncounterSet.TheEssexCountyExpress
    2
    (Static 0)
    NoSymbol
    []
    (singleton Train)

instance HasCount ClueCount env LocationId => HasModifiersFor env DiningCar where
  getModifiersFor _ target (DiningCar location@LocationAttrs {..})
    | isTarget location target = case lookup LeftOf locationDirections of
      Just leftLocation -> do
        clueCount <- unClueCount <$> getCount leftLocation
        pure $ toModifiers
          location
          [ Blocked | not locationRevealed && clueCount > 0 ]
      Nothing -> pure []
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env DiningCar where
  getActions iid window (DiningCar attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env DiningCar where
  runMessage msg (DiningCar attrs) = case msg of
    RevealLocation (Just iid) lid | lid == locationId attrs -> do
      unshiftMessage
        (FindAndDrawEncounterCard iid (EncounterCardMatchByCardCode "02182"))
      DiningCar <$> runMessage msg attrs
    _ -> DiningCar <$> runMessage msg attrs
