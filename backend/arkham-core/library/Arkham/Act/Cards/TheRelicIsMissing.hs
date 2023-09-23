module Arkham.Act.Cards.TheRelicIsMissing (
  TheRelicIsMissing (..),
  theRelicIsMissing,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message

newtype TheRelicIsMissing = TheRelicIsMissing ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theRelicIsMissing :: ActCard TheRelicIsMissing
theRelicIsMissing =
  act (1, A) TheRelicIsMissing Cards.theRelicIsMissing
    $ Just
    $ GroupClueCost (PerPlayer 1)
    $ LocationWithTitle "Miskatonic University"

instance RunMessage TheRelicIsMissing where
  runMessage msg a@(TheRelicIsMissing attrs) = case msg of
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      placeEztliExhibit <- placeLocationCard_ Locations.eztliExhibit
      pushAll
        [ placeEztliExhibit
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
      pure a
    _ -> TheRelicIsMissing <$> runMessage msg attrs
