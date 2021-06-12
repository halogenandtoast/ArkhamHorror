module Arkham.Types.Location.Cards.AudubonPark where

import Arkham.Prelude

import Arkham.Types.Classes
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Trait

newtype AudubonPark = AudubonPark LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

audubonPark :: LocationId -> AudubonPark
audubonPark = AudubonPark . (victoryL ?~ 1) . baseAttrs
  "81011"
  "Audubon Park"
  EncounterSet.CurseOfTheRougarou
  3
  (PerPlayer 1)
  Squiggle
  [Triangle, Squiggle]
  [Riverside]

instance HasModifiersFor env AudubonPark where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env AudubonPark where
  getActions i window (AudubonPark attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env AudubonPark where
  runMessage msg l@(AudubonPark attrs@LocationAttrs {..}) = case msg of
    EnemyEvaded iid eid | eid `member` locationEnemies ->
      l <$ unshiftMessage (RandomDiscard iid)
    _ -> AudubonPark <$> runMessage msg attrs
