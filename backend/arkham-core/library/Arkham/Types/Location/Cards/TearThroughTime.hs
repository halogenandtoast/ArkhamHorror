module Arkham.Types.Location.Cards.TearThroughTime
  ( tearThroughTime
  , TearThroughTime(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (tearThroughTime)
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.InvestigatorId
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Window

newtype TearThroughTime = TearThroughTime LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tearThroughTime :: LocationId -> TearThroughTime
tearThroughTime = TearThroughTime . baseAttrs
  Cards.tearThroughTime
  2
  (PerPlayer 2)
  Moon
  [Circle, Plus, Squiggle]

instance HasModifiersFor env TearThroughTime where
  getModifiersFor = noModifiersFor

resignAction :: SourceEntity a => InvestigatorId -> a -> Message
resignAction iid a = ActivateCardAbilityAction
  iid
  (mkAbility
    (toSource a)
    99
    (ActionAbility (Just Action.Resign) (Costs [ActionCost 1, ClueCost 2]))
  )

instance ActionRunner env => HasActions env TearThroughTime where
  getActions iid NonFast (TearThroughTime attrs) =
    withBaseActions iid NonFast attrs $ pure [resignAction iid attrs]
  getActions iid window (TearThroughTime attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env TearThroughTime where
  runMessage msg (TearThroughTime attrs) =
    TearThroughTime <$> runMessage msg attrs
