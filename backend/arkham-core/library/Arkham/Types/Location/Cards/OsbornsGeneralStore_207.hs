module Arkham.Types.Location.Cards.OsbornsGeneralStore_207
  ( osbornsGeneralStore_207
  , OsbornsGeneralStore_207(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (osbornsGeneralStore_207)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait
import Arkham.Types.Window

newtype OsbornsGeneralStore_207 = OsbornsGeneralStore_207 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

osbornsGeneralStore_207 :: LocationCard OsbornsGeneralStore_207
osbornsGeneralStore_207 = location
  OsbornsGeneralStore_207
  Cards.osbornsGeneralStore_207
  3
  (PerPlayer 1)
  Circle
  [Moon, Square]

ability :: LocationAttrs -> Ability
ability attrs = mkAbility
  (toSource attrs)
  1
  (ActionAbility Nothing $ Costs [ActionCost 1, ResourceCost 1])

instance ActionRunner env => HasAbilities env OsbornsGeneralStore_207 where
  getAbilities iid window@(Window Timing.When NonFast) (OsbornsGeneralStore_207 attrs)
    | locationRevealed attrs
    = withBaseActions iid window attrs $ pure [locationAbility (ability attrs)]
  getAbilities iid window@(Window Timing.When FastPlayerWindow) (OsbornsGeneralStore_207 attrs)
    | locationRevealed attrs
    = withBaseActions iid window attrs $ pure
      [ drawCardUnderneathAction attrs
      | iid `on` attrs && locationClues attrs == 0
      ]
  getAbilities iid window (OsbornsGeneralStore_207 attrs) =
    getAbilities iid window attrs

instance LocationRunner env => RunMessage env OsbornsGeneralStore_207 where
  runMessage msg l@(OsbornsGeneralStore_207 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ push
      (SearchTopOfDeck
        iid
        source
        (InvestigatorTarget iid)
        3
        [Item]
        (ShuffleBackIn $ DrawFound iid)
      )
    _ -> OsbornsGeneralStore_207 <$> runMessage msg attrs
