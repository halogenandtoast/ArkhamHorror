module Arkham.Types.Location.Cards.TheEdgeOfTheUniverse
  ( theEdgeOfTheUniverse
  , TheEdgeOfTheUniverse(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Phase
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Trait

newtype TheEdgeOfTheUniverse = TheEdgeOfTheUniverse LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEdgeOfTheUniverse :: LocationId -> TheEdgeOfTheUniverse
theEdgeOfTheUniverse lid = TheEdgeOfTheUniverse $ baseAttrs
  lid
  "02321"
  (Name "The Edge of the Universe" Nothing)
  EncounterSet.LostInTimeAndSpace
  2
  (PerPlayer 2)
  Moon
  [Plus, Squiggle]
  [Otherworld]

instance HasPhase env => HasModifiersFor env TheEdgeOfTheUniverse where
  getModifiersFor _ (InvestigatorTarget iid) (TheEdgeOfTheUniverse attrs)
    | iid `on` attrs = do
      phase <- getPhase
      pure $ toModifiers attrs [ CannotDrawCards | phase == UpkeepPhase ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env TheEdgeOfTheUniverse where
  getActions iid window (TheEdgeOfTheUniverse attrs) = do
    actions <- getActions iid window attrs
    clueCount <- unClueCount <$> getCount iid
    pure $ if clueCount >= 2
      then actions
      else filter
        (\case
          MoveAction{} -> False
          _ -> True
        )
        actions

instance LocationRunner env => RunMessage env TheEdgeOfTheUniverse where
  runMessage msg (TheEdgeOfTheUniverse attrs) =
    TheEdgeOfTheUniverse <$> runMessage msg attrs
