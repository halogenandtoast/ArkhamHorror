module Arkham.Types.Location.Cards.OsbornsGeneralStore_206
  ( osbornsGeneralStore_206
  , OsbornsGeneralStore_206(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (osbornsGeneralStore_206)
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype OsbornsGeneralStore_206 = OsbornsGeneralStore_206 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

osbornsGeneralStore_206 :: LocationId -> OsbornsGeneralStore_206
osbornsGeneralStore_206 = OsbornsGeneralStore_206 . baseAttrs
  Cards.osbornsGeneralStore_206
  2
  (PerPlayer 1)
  Circle
  [Moon, Square]

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
