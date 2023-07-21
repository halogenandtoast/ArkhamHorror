module Arkham.Act.Cards.PathsIntoTwilight (
  PathsIntoTwilight (..),
  pathsIntoTwilight,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection

newtype PathsIntoTwilight = PathsIntoTwilight ActAttrs
  deriving anyclass (IsAct)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

pathsIntoTwilight :: ActCard PathsIntoTwilight
pathsIntoTwilight =
  act
    (3, A)
    PathsIntoTwilight
    Cards.pathsIntoTwilight
    (Just $ GroupClueCost (PerPlayer 3) Anywhere)

instance HasModifiersFor PathsIntoTwilight where
  getModifiersFor (LocationTarget lid) (PathsIntoTwilight a) = do
    mInFrontOf <- field LocationInFrontOf lid
    pure $
      toModifiers
        a
        [ ConnectedToWhen (LocationWithId lid) $
          NotLocation (LocationWithId lid)
            <> LocationIsInFrontOf (InvestigatorWithId iid)
        | iid <- maybeToList mInFrontOf
        ]
  getModifiersFor _ _ = pure []

instance RunMessage PathsIntoTwilight where
  runMessage msg a@(PathsIntoTwilight attrs) = case msg of
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      (witchesCircleId, placeWitchesCircle) <-
        placeLocationCard
          Locations.witchesCircle
      lead <- getLead
      pushAll
        [ placeWitchesCircle
        , Revelation lead (toSource witchesCircleId)
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
      pure a
    _ -> PathsIntoTwilight <$> runMessage msg attrs
