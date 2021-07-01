module Arkham.Types.Location.Cards.ForgottenMarsh where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (forgottenMarsh)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message

newtype ForgottenMarsh = ForgottenMarsh LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forgottenMarsh :: LocationId -> ForgottenMarsh
forgottenMarsh = ForgottenMarsh . baseAttrs
  Cards.forgottenMarsh
  2
  (Static 0)
  Diamond
  [Moon, Square, Triangle, Hourglass]

instance HasModifiersFor env ForgottenMarsh where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ForgottenMarsh where
  getActions i window (ForgottenMarsh attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env ForgottenMarsh where
  runMessage msg l@(ForgottenMarsh attrs@LocationAttrs {..}) = case msg of
    Will (MoveTo iid lid)
      | iid `elem` locationInvestigators && lid /= locationId -> l
      <$ unshiftMessage (SpendResources iid 2)
    _ -> ForgottenMarsh <$> runMessage msg attrs
