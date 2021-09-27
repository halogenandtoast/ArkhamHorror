module Arkham.Types.Act.Cards.Trapped where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Matcher (LocationMatcher(..))
import Arkham.Types.Message
import Arkham.Types.Target

newtype Trapped = Trapped ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

trapped :: ActCard Trapped
trapped =
  act (1, A) Trapped Cards.trapped (Just $ GroupClueCost (PerPlayer 2) Anywhere)

instance ActRunner env => RunMessage env Trapped where
  runMessage msg a@(Trapped attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      studyId <- getJustLocationIdByName "Study"
      enemyIds <- getSetList studyId

      hallway <- getSetAsideCard Locations.hallway
      cellar <- getSetAsideCard Locations.cellar
      attic <- getSetAsideCard Locations.attic
      parlor <- getSetAsideCard Locations.parlor

      let hallwayId = LocationId $ toCardId hallway

      a <$ pushAll
        ([ PlaceLocation hallway
         , PlaceLocation cellar
         , PlaceLocation attic
         , PlaceLocation parlor
         ]
        <> map (Discard . EnemyTarget) enemyIds
        <> [ RevealLocation Nothing hallwayId
           , MoveAllTo (toSource attrs) hallwayId
           , RemoveLocation studyId
           , NextAct aid "01109"
           ]
        )
    _ -> Trapped <$> runMessage msg attrs
