module Arkham.Types.Location.Cards.OsbornsGeneralStore_206
  ( osbornsGeneralStore_206
  , OsbornsGeneralStore_206(..)
  )
where


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype OsbornsGeneralStore_206 = OsbornsGeneralStore_206 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

osbornsGeneralStore_206 :: OsbornsGeneralStore_206
osbornsGeneralStore_206 = OsbornsGeneralStore_206 $ baseAttrs
  "02206"
  (Name "Osborn's General Store" Nothing)
  EncounterSet.BloodOnTheAltar
  2
  (PerPlayer 1)
  Circle
  [Moon, Square]
  [Dunwich]

instance HasModifiersFor env OsbornsGeneralStore_206 where
  getModifiersFor _ (InvestigatorTarget iid) (OsbornsGeneralStore_206 attrs) =
    pure $ toModifiers
      attrs
      [ CannotGainResources | iid `member` locationInvestigators attrs ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env OsbornsGeneralStore_206 where
  getActions = withDrawCardUnderneathAction

instance LocationRunner env => RunMessage env OsbornsGeneralStore_206 where
  runMessage msg (OsbornsGeneralStore_206 attrs) =
    OsbornsGeneralStore_206 <$> runMessage msg attrs
