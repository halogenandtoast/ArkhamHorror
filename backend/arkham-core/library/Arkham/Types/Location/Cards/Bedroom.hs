module Arkham.Types.Location.Cards.Bedroom where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (bedroom)
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message

newtype Bedroom = Bedroom LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bedroom :: LocationId -> Bedroom
bedroom = Bedroom . baseAttrs
  Cards.bedroom
  2
  (PerPlayer 1)
  Heart
  [T]

instance HasModifiersFor env Bedroom where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Bedroom where
  getActions i window (Bedroom attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env Bedroom where
  runMessage msg l@(Bedroom attrs) = case msg of
    After (FailedSkillTest iid (Just Action.Investigate) _ target _ _)
      | isTarget attrs target -> l <$ unshiftMessage (RandomDiscard iid)
    _ -> Bedroom <$> runMessage msg attrs
