module Arkham.Types.Location.Cards.Attic where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (attic)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Source

newtype Attic = Attic LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

attic :: LocationCard Attic
attic = location Attic Cards.attic 1 (PerPlayer 2) Triangle [Square]

instance HasModifiersFor env Attic

instance HasAbilities env Attic where
  getAbilities i window (Attic attrs) = getAbilities i window attrs

instance (LocationRunner env) => RunMessage env Attic where
  runMessage msg a@(Attic attrs@LocationAttrs { locationId }) = case msg of
    AfterEnterLocation iid lid | lid == locationId -> a <$ push
      (InvestigatorAssignDamage iid (LocationSource locationId) DamageAny 0 1)
    _ -> Attic <$> runMessage msg attrs
