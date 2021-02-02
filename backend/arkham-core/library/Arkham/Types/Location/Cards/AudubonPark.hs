module Arkham.Types.Location.Cards.AudubonPark where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype AudubonPark = AudubonPark Attrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

audubonPark :: AudubonPark
audubonPark = AudubonPark $ base { locationVictory = Just 1 }
 where
  base = baseAttrs
    "81011"
    (Name "Audubon Park" Nothing)
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
  runMessage msg l@(AudubonPark attrs@Attrs {..}) = case msg of
    EnemyEvaded iid eid | eid `member` locationEnemies ->
      l <$ unshiftMessage (RandomDiscard iid)
    _ -> AudubonPark <$> runMessage msg attrs
