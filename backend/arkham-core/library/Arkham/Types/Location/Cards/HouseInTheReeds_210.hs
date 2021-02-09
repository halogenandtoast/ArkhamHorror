module Arkham.Types.Location.Cards.HouseInTheReeds_210
  ( houseInTheReeds_210
  , HouseInTheReeds_210(..)
  ) where


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype HouseInTheReeds_210 = HouseInTheReeds_210 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

houseInTheReeds_210 :: HouseInTheReeds_210
houseInTheReeds_210 = HouseInTheReeds_210 $ baseAttrs
  "02210"
  (Name "House in the Reeds" Nothing)
  EncounterSet.BloodOnTheAltar
  2
  (PerPlayer 1)
  Squiggle
  [Diamond, Moon]
  [Dunwich]

instance HasModifiersFor env HouseInTheReeds_210 where
  getModifiersFor _ (InvestigatorTarget iid) (HouseInTheReeds_210 attrs) =
    pure $ toModifiers
      attrs
      [ CannotPlay [(EventType, mempty)]
      | iid `elem` locationInvestigators attrs
      ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env HouseInTheReeds_210 where
  getActions = withDrawCardUnderneathAction

instance LocationRunner env => RunMessage env HouseInTheReeds_210 where
  runMessage msg (HouseInTheReeds_210 attrs) =
    HouseInTheReeds_210 <$> runMessage msg attrs
