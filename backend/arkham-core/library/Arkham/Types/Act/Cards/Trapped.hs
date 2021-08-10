module Arkham.Types.Act.Cards.Trapped where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Location.Cards as Locations
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Target

newtype Trapped = Trapped ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasActions, HasModifiersFor env)

trapped :: ActCard Trapped
trapped =
  act (1, A) Trapped Cards.trapped (Just $ GroupClueCost (PerPlayer 2) Nothing)

instance ActRunner env => RunMessage env Trapped where
  runMessage msg a@(Trapped attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      studyId <- getJustLocationIdByName "Study"
      enemyIds <- getSetList studyId
      hallwayId <- getRandom
      atticId <- getRandom
      cellarId <- getRandom
      parlorId <- getRandom
      a <$ pushAll
        ([ PlaceLocation hallwayId Locations.hallway
         , PlaceLocation cellarId Locations.cellar
         , PlaceLocation atticId Locations.attic
         , PlaceLocation parlorId Locations.parlor
         ]
        <> map (Discard . EnemyTarget) enemyIds
        <> [ RevealLocation Nothing hallwayId
           , MoveAllTo hallwayId
           , RemoveLocation studyId
           , NextAct aid "01109"
           ]
        )
    _ -> Trapped <$> runMessage msg attrs
