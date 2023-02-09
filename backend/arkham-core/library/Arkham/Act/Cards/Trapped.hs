module Arkham.Act.Cards.Trapped where

import Arkham.Prelude

import Arkham.Act.Types
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher (LocationMatcher(..), enemyAt)
import Arkham.Message
import Arkham.Target

newtype Trapped = Trapped ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

trapped :: ActCard Trapped
trapped =
  act (1, A) Trapped Cards.trapped (Just $ GroupClueCost (PerPlayer 2) Anywhere)

instance RunMessage Trapped where
  runMessage msg a@(Trapped attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ _ | aid == actId && onSide B attrs -> do
      studyId <- selectJust $ LocationWithTitle "Study"
      enemyIds <- selectList $ enemyAt studyId

      hallway <- getSetAsideCard Locations.hallway
      cellar <- getSetAsideCard Locations.cellar
      attic <- getSetAsideCard Locations.attic
      parlor <- getSetAsideCard Locations.parlor

      let hallwayId = LocationId $ toCardId hallway

      pushAll $
        [ PlaceLocation hallway
        , PlaceLocation cellar
        , PlaceLocation attic
        , PlaceLocation parlor
        ]
       <> map (Discard (toSource attrs) . EnemyTarget) enemyIds
       <> [ RevealLocation Nothing hallwayId
          , MoveAllTo (toSource attrs) hallwayId
          , RemoveLocation studyId
          , AdvanceActDeck actDeckId (toSource attrs)
          ]
      pure a
    _ -> Trapped <$> runMessage msg attrs
