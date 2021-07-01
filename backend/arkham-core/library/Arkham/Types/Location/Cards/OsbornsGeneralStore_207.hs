module Arkham.Types.Location.Cards.OsbornsGeneralStore_207
  ( osbornsGeneralStore_207
  , OsbornsGeneralStore_207(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (osbornsGeneralStore_207)
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
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Window

newtype OsbornsGeneralStore_207 = OsbornsGeneralStore_207 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

osbornsGeneralStore_207 :: LocationId -> OsbornsGeneralStore_207
osbornsGeneralStore_207 = OsbornsGeneralStore_207 . baseAttrs
  Cards.osbornsGeneralStore_207
  3
  (PerPlayer 1)
  Circle
  [Moon, Square]

instance HasModifiersFor env OsbornsGeneralStore_207 where
  getModifiersFor = noModifiersFor

ability :: LocationAttrs -> Ability
ability attrs = mkAbility
  (toSource attrs)
  1
  (ActionAbility Nothing $ Costs [ActionCost 1, ResourceCost 1])

instance ActionRunner env => HasActions env OsbornsGeneralStore_207 where
  getActions iid NonFast (OsbornsGeneralStore_207 attrs)
    | locationRevealed attrs = withBaseActions iid NonFast attrs
    $ pure [ActivateCardAbilityAction iid (ability attrs)]
  getActions iid FastPlayerWindow (OsbornsGeneralStore_207 attrs)
    | locationRevealed attrs = withBaseActions iid FastPlayerWindow attrs $ pure
      [ drawCardUnderneathAction iid attrs
      | iid `on` attrs && locationClues attrs == 0
      ]
  getActions iid window (OsbornsGeneralStore_207 attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env OsbornsGeneralStore_207 where
  runMessage msg l@(OsbornsGeneralStore_207 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessage
        (SearchTopOfDeck iid (InvestigatorTarget iid) 3 [Item] ShuffleBackIn)
    _ -> OsbornsGeneralStore_207 <$> runMessage msg attrs
